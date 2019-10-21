import re

from typing import Any, Callable, List, Tuple, Dict, Set, Union, Optional
from functools import lru_cache
from xml.etree import ElementTree as ET # type: ignore

import xmlschema # type: ignore
from xmlschema.validators import ( # type: ignore
    XsdAttribute,
    XsdAtomicBuiltin,
    XsdAtomicRestriction,
    XsdComplexType,
    XsdElement,
    XsdGroup,
    XsdSimpleType,
    XsdList,
    XsdType,
    XsdUnion,
    XMLSchema10,
)

from . import utils, cpp_templates as tpl
from .dfa import dfa_from_group, XsdDFA

class UxsdType:
	"""An XSD type which corresponds to a type in C++."""
	name: str
	def __init__(self):
		raise TypeError("Use child types instead.")
	@property
	def cpp(self) -> str:
		raise NotImplementedError("You should implement type.cpp.")

class UxsdSourcable:
	"""A type for which you can get the corresponding XSD source."""
	xml_elem: ET.Element
	def __init__(self):
		raise TypeError("Use child types instead.")
	@property
	def source(self) -> str:
		# Fixes ET's indentation and removes the namespace attribute.
		out = ET.tostring(self.xml_elem).decode()
		out = re.sub(r'xmlns:xs="(.*?)" ', r'', out)
		return re.sub(r"  (.*)", r"\1", out)

class UxsdSimple(UxsdType):
	pass

class UxsdUnion(UxsdSimple, UxsdSourcable):
	member_types: List[UxsdSimple]
	def __init__(self, name, member_types, xml_elem):
		self.name = name
		self.member_types = member_types
		self.xml_elem = xml_elem
	@property
	def cpp(self) -> str:
		return "union_%s" % self.name

class UxsdEnum(UxsdSimple):
	enumeration: List[str]
	def __init__(self, name, enumeration):
		self.name = name
		self.enumeration = enumeration
	@property
	def cpp(self) -> str:
		return "enum_%s" % self.name

class UxsdAtomic(UxsdSimple):
	def __init__(self):
		raise TypeError("Use child types instead.")
	@property
	def cpp(self) -> str:
		return tpl.atomic_builtins[self.name]
	@property
	def cpp_load_format(self) -> str:
		return tpl.atomic_builtin_load_formats[self.name]

class UxsdNumber(UxsdAtomic):
	def __init__(self, name):
		self.name = name

class UxsdString(UxsdAtomic):
	def __init__(self):
		self.name = "string"

class UxsdAttribute:
	name: str
	default_value: Optional[str]
	optional: bool
	type: UxsdSimple
	def __init__(self, name, default_value, optional, type):
		self.name = name
		self.default_value = default_value
		self.optional = optional
		self.type = type

class UxsdElement(UxsdSourcable):
	name: str
	many: bool
	optional: bool
	type: UxsdType
	def __init__(self, name, many, optional, type, xml_elem):
		self.name = name
		self.many = many
		self.optional = optional
		self.type = type
		self.xml_elem = xml_elem

class UxsdContentType:
	def __init__(self):
		raise TypeError("Use child types instead.")

class UxsdAll(UxsdContentType):
	children: List[UxsdElement]
	def __init__(self, children):
		self.children = children

class UxsdDfa(UxsdContentType):
	children: List[UxsdElement]
	dfa: XsdDFA
	def __init__(self, children, dfa):
		self.children = children
		self.dfa = dfa

class UxsdLeaf(UxsdContentType):
	type: UxsdSimple
	def __init__(self, type):
		self.type = type

class UxsdComplex(UxsdType, UxsdSourcable):
	"""An XSD complex type. It has attributes and content."""
	attrs: List[UxsdAttribute]
	content: Optional[UxsdContentType]
	def __init__(self, name, attrs, content, xml_elem):
		self.name = name
		self.attrs = attrs
		self.content = content
		self.xml_elem = xml_elem
	@property
	def cpp(self) -> str:
		return "t_%s" % self.name

UxsdAny = Union[UxsdType, UxsdContentType, UxsdElement, UxsdAttribute]

class UxsdSchema:
	"""A XSD schema tree derived from xmlschema's tree.

	It provides convenient data structures, such as ordered
	access to children and attributes, C++ type names of complex types etc.
	This tree assumes it's immutable!
	"""
	# All user-defined complex types and root elements.
	complex_types: List[UxsdComplex] = []
	root_elements: List[UxsdElement] = []

	# Complex types found inside elements. They are not found in the global map,
	# so we have to reserve them while traversing types in the global map
	# and generate them afterwards.
	anonymous_complex_types: List[UxsdComplex] = []

	# Enumerations and unions, which need C++ declarations of their own.
	enums: List[UxsdEnum] = []
	unions: List[UxsdUnion] = []

	# Simple types found inside unions.
	# We generate a special "type_tag" enum from this.
	simple_types_in_unions: List[UxsdSimple] = []

	# In C++ code, we allocate global pools for types
	# which may occur more than once, so that we can avoid
	# frequent allocations.
	pool_types: List[UxsdType] = []

	# A special pool is generated for strings.
	has_string: bool = False

	# Build a UxsdSchema out of an XsdSchema using a recursive walk.
	# We cache the results in a functools.lru_cache of unbounded size to
	# guarantee that an Xsd* object will always give the same Uxsd* object
	# and will invoke the side effects(fill global lists) only on the first time.
	@lru_cache(maxsize=None)
	def visit_group(self, t: XsdGroup, many=False, optional=False) -> List[UxsdElement]:
		out: List[UxsdElement] = []
		if t.occurs[1] is None or t.occurs[1] > 1: many = True
		if t.occurs[0] is 0: optional = True
		if not many and t.model == "choice": optional = True
		for e in t._group:
			if isinstance(e, XsdGroup):
				out += self.visit_group(e, many, optional)
			elif isinstance(e, XsdElement):
				out.append(self.visit_element(e, many, optional))
			else:
				raise NotImplementedError("I don't know what to do with group member %s." % e)
		return out

	@lru_cache(maxsize=None)
	def visit_element(self, t: XsdElement, many=False, optional=False) -> UxsdElement:
		if t.occurs[1] is None or t.occurs[1] > 1: many = True
		if t.occurs[0] is 0: optional = True

		type: UxsdType
		if isinstance(t.type, XsdComplexType):
			type = self.visit_complex_type(t.type)
		else:
			type = self.visit_simple_type(t.type)

		name = t.name
		if many:
			self.pool_types.append(type)
		xml_elem = t.schema_elem
		return UxsdElement(name, many, optional, type, xml_elem)

	# Only enumerations are supported.
	@lru_cache(maxsize=None)
	def visit_restriction(self, t: XsdAtomicRestriction) -> Union[UxsdSimple, UxsdEnum]:
		if len(t.validators) == 0:
			return self.visit_simple_type(t.base_type)
		elif len(t.validators) == 1:
			# Possibly member of an XsdList or XsdUnion if it doesn't have a name attribute.
			name = t.name if t.name else t.parent.name
			enumeration = t.validators[0].enumeration
			out = UxsdEnum(name, enumeration)
			self.enums.append(out)
			return out
		else:
			raise NotImplementedError("Only simple enumerations are supported.")

	@lru_cache(maxsize=None)
	def visit_union(self, t: XsdUnion) -> UxsdUnion:
		member_types = []
		for m in t.member_types:
			x = self.visit_simple_type(m)
			member_types.append(x)
			self.simple_types_in_unions.append(x)
		xml_elem = t.schema_elem
		out = UxsdUnion(t.name, member_types, xml_elem)
		self.unions.append(out)
		return out

	@lru_cache(maxsize=None)
	def visit_simple_type(self, t: XsdSimpleType) -> UxsdSimple:
		# Remove w3.org namespace from built-in type names.
		if "w3.org" in t.name:
			name = t.name.split("}")[1]
		if isinstance(t, XsdAtomicBuiltin):
			if name == "string":
				self.has_string = True
				return UxsdString()
			else:
				return UxsdNumber(name)
		elif isinstance(t, XsdList):
			# Just read xs:lists into a string for now.
			# That simplifies validation and keeps heap allocation to nodes only.
			# VPR just reads list types into a string, too.
			self.has_string = True
			return UxsdString()
		elif isinstance(t, XsdAtomicRestriction):
			return self.visit_restriction(t)
		elif isinstance(t, XsdUnion):
			return self.visit_union(t)
		else:
			raise NotImplementedError("Unknown XsdSimpleType %s." % t)

	@lru_cache(maxsize=None)
	def visit_attribute(self, a: XsdAttribute) -> UxsdAttribute:
		if a.use == "optional":
			optional = True
		elif a.use == "required":
			optional = False
		else:
			raise NotImplementedError("I don't know what to do with attribute use=%s." % a.use)
		default_value = getattr(a, "default", None)
		optional = True if a.use == "optional" else False
		type = self.visit_simple_type(a.type)
		return UxsdAttribute(a.name, default_value, optional, type)

	@lru_cache(maxsize=None)
	def visit_complex_type(self, t: XsdComplexType) -> UxsdComplex:
		if t.name is None:
			# Anonymous complex type. Assign its element's name.
			name = t.parent.name
		else:
			name = t.name

		# Remove possible duplicates.
		# https://stackoverflow.com/a/39835527
		attrs = sorted([self.visit_attribute(a) for a in t.attributes.values()], key=lambda x: x.name)

		content: Optional[UxsdContentType] = None
		if isinstance(t.content_type, XsdGroup) and len(t.content_type._group) > 0:
			if t.content_type.model == "all":
				children = self.visit_group(t.content_type)
				content = UxsdAll(children)
			elif t.content_type.model in ["choice", "sequence"]:
				children = self.visit_group(t.content_type)
				dfa = dfa_from_group(t.content_type)
				content = UxsdDfa(children, dfa)
			else:
				raise NotImplementedError("Model group %s is not supported." % t.content_type.model)
		elif t.has_simple_content():
			type = self.visit_simple_type(t.content_type)
			content = UxsdLeaf(type)

		xml_elem = t.schema_elem
		out = UxsdComplex(name, attrs, content, xml_elem)
		if t.name is None:
			self.anonymous_complex_types.append(out)
		return out

	def __init__(self, parent: XMLSchema10) -> None:
		for k, v in parent.types.items():
			if "w3.org" not in k and isinstance(v, XsdComplexType):
				self.complex_types.append(self.visit_complex_type(v))
		for v in parent.elements.values():
			self.root_elements.append(self.visit_element(v))

		# The visit* functions have side effects, they update schema-wide lists.
		# Remove duplicates from schema-wide lists while preserving order.
		self.enums = list(dict.fromkeys(self.enums))
		self.unions = list(dict.fromkeys(self.unions))
		self.simple_types_in_unions = list(dict.fromkeys(self.simple_types_in_unions))
		self.pool_types = list(dict.fromkeys(self.pool_types))

		# Collect complex types and sort by tree height.
		def key_type(x: UxsdType, visited=None) -> int:
			if not visited: visited=set()
			if x in visited: return 0
			else: visited.add(x)
			if isinstance(x, UxsdComplex) and isinstance(x.content, (UxsdAll, UxsdDfa)):
				tree_heights: List[int] = []
				for child in x.content.children:
					if isinstance(child.type, UxsdComplex):
						tree_heights.append(key_type(x, visited))
					else:
						tree_heights.append(1)
				return max(tree_heights) + 1
			else:
				return 1

		self.complex_types += self.anonymous_complex_types
		self.complex_types.sort(key=key_type)
		self.pool_types.sort(key=key_type)

	@property
	def has_dfa(self) -> bool:
		x = [True for x in self.complex_types if isinstance(x.content, UxsdDfa)]
		return any(x)

	@property
	def has_all(self) -> bool:
		x = [True for x in self.complex_types if isinstance(x.content, UxsdAll)]
		return any(x)

	@property
	def has_attr(self) -> bool:
		x = [True for x in self.complex_types if x.attrs]
		return any(x)
