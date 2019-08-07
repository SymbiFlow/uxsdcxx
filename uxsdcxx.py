# Script to convert an XSD schema to a C++ struct definition and
# a parser based on PugiXML.

from pprint import pprint

import hashlib
import os
import re
import sys
import xmlschema
from xmlschema.validators import (
    XsdAttribute,
    XsdAtomicBuiltin,
    XsdAtomicRestriction,
    XsdComplexType,
    XsdElement,
    XsdGroup,
    XsdSimpleType,
    XsdList,
    XsdType,
    XsdUnion
)

from itertools import chain
# Note that Python type annotations are just annotations and
# have no runtime effects. But they are useful for documentation.
from typing import List, Tuple, Dict, Set, Union

from dfa import dfa_from_group
from third_party import triehash

# See https://www.obj-sys.com/docs/xbv23/CCppUsersGuide/ch04.html.
atomic_builtins = {
	"string": "const char *",
	"boolean": "bool",
	"float": "float",
	"decimal": "int",
	"integer": "int",
	"nonPositiveInteger": "int",
	"negativeInteger": "int",
	"long": "long",
	"int": "int",
	"short": "short",
	"byte": "char",
	"nonNegativeInteger": "unsigned int",
	"unsignedLong": "unsigned long",
	"unsignedInt": "unsigned int",
	"unsignedShort": "unsigned short",
	"unsignedByte": "unsigned byte",
	"positiveInteger": "unsigned int",
	"double": "double",
}

atomic_builtin_load_formats = {
	"string": "strdup(%s)",
	"boolean": "std::strtol(%s, NULL, 10)",
	"float": "std::strtof(%s, NULL)",
	"decimal": "std::strtol(%s, NULL, 10)",
	"integer": "std::strtol(%s, NULL, 10)",
	"nonPositiveInteger": "std::strtol(%s, NULL, 10)",
	"negativeInteger": "std::strtol(%s, NULL, 10)",
	"long": "std::strtoll(%s, NULL, 10)",
	"int": "std::strtol(%s, NULL, 10)",
	"short": "std::strtol(%s, NULL, 10)",
	"byte": "std::strtol(%s, NULL, 10)",
	"nonNegativeInteger": "std::strtoul(%s, NULL, 10)",
	"unsignedLong": "std::strtoull(%s, NULL, 10)",
	"unsignedInt": "std::strtoul(%s, NULL, 10)",
	"unsignedShort": "std::strtoul(%s, NULL, 10)",
	"unsignedByte": "std::strtoul(%s, NULL, 10)",
	"positiveInteger": "std::strtoul(%s, NULL, 10)",
	"double": "std::strtod(%s, NULL)",
}

cpp_keywords = ["alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept",
			"auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char8_t", "char16_t", "char32_t", "class",
			"compl", "concept", "const", "consteval", "constexpr", "const_cast", "continue", "co_await", "co_return",
			"co_yield", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit",
			"export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
			"namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
			"protected", "public", "reflexpr", "register", "reinterpret_cast", "requires", "return", "short", "signed",
			"sizeof", "static", "static_assert", "static_cast", "struct", "switch", "synchronized", "template", "this",
			"thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
			"virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"]

#

file_name = sys.argv[1]
base_name = os.path.splitext(os.path.basename(file_name))[0]
header_file_name = base_name + "_uxsdcxx.h"
impl_file_name = base_name + "_uxsdcxx.cpp"

# Read in the schema- here we go!
schema = xmlschema.validators.XMLSchema10(file_name)

# Complex types found inside elements. They are not found in the global map,
# so we have to reserve them while traversing types in the global map
# and generate them afterwards.
anonymous_complex_types = []

# Enumerations and unions found inside elements.
enums = []
unions = []

# Simple types found inside unions.
# We generate a special "types" enum from this, to put in all tagged union type definitions.
simple_types = []

# In C++ code, we have to allocate global pools for types which can occur more than once,
# so that we can avoid lots of reallocs on the heap.
pool_types = []

# Get all global user-defined types.
types: List[XsdComplexType] = [v for k, v in schema.types.items() if "w3.org" not in k and isinstance(v, XsdComplexType)]
root_elements: List[XsdElement] = [v for v in schema.elements.values()]

#

# https://stackoverflow.com/a/3431838
def md5(fname):
    hash_md5 = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()

def checked(x: str) -> str:
	"""Check against keywords, warn and rename if necessary."""
	if x in cpp_keywords:
		print("%s is a C++ keyword. Changing it to %s_." % (x, x), file=sys.stderr)
		return x + "_"
	return x

def to_cpp_type(x: str) -> str:
	"""Convert type name to C++ type. Elements can have builtins as types."""
	if x in atomic_builtins: return atomic_builtins[x]
	else: return "t_%s" % x

def to_token(x: str) -> str:
	return re.sub(r"[^a-zA-Z0-9_]", "_", x).upper()

def to_union_member_type(x: str) -> str:
	return "as_%s" % re.sub(r"[^a-zA-Z0-9_]", "_", x)

def indent(x: str, n: int=1) -> str:
	return "\n".join(["\t"*n + line if line else "" for line in x.split("\n")])

def pluralize(x: str) -> str:
	"""Rudimentary pluralization function. It's used to name lists of things."""
	if x[-2:] in ["sh", "ch", "ss"]: return x+"es"
	elif x[-1:] in ["s", "x", "z"]: return x+"es"
	else: return x+"s"

#

# Annotate the xmlschema tree with convenient data structures, such as ordered
# access to children and attributes, C++ types of complex types etc.
# Add the relevant types to the lists for anonymous complex types, enums and unions.

def anno_element(t: XsdElement, many=False, optional=False) -> None:
	if getattr(t, "cpp_type", None) is not None: return

	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	if t.occurs[0] is 0: optional = True
	if not t.type.name:
		t.type.name = t.name
		anonymous_complex_types.append(t.type)

	if isinstance(t.type, XsdComplexType):
		anno_complex_type(t.type)
		# We have a recursive type and annodecl_complex_type did nothing.
		if not getattr(t.type, "cpp_type", None): return
	else:
		anno_simple_type(t.type)

	t.many = many
	t.optional = optional
	t.cpp_type = t.type.cpp_type
	if many:
		pool_types.append(t.type)

# Returns a list of elements in the group to aid complex type annotation.
def anno_group(t: XsdGroup, many=False, optional=False) -> List[XsdElement]:
	out = []
	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	if t.occurs[0] is 0: optional = True
	for e in t._group:
		if isinstance(e, XsdGroup):
			out += anno_group(e, many, optional)
		elif isinstance(e, XsdElement):
			out += [e]
			anno_element(e, many, optional)
		else:
			raise NotImplementedError("I don't know what to do with group member %s." % e)
	return out

# Only enumerations are supported as restrictions.
def anno_restriction(t: XsdAtomicRestriction) -> None:
	assert len(t.validators) == 1, "I can only handle simple enumerations."
	# Possibly member of an XsdList or XsdUnion.
	if not t.name: t.name = t.parent.name
	t.cpp_type = "enum_%s" % t.name
	enums.append(t)

def anno_union(t: XsdUnion) -> None:
	t.cpp_type = "union_%s" % t.name
	for m in t.member_types:
		anno_simple_type(m)
		simple_types.append(m)
	unions.append(t)

def anno_simple_type(t: XsdSimpleType) -> None:
	"""Annotate a simple type with a C++ type name.

	Also remove the namespace prefix from its name assigned by xmlschema:
	https://bugs.python.org/issue18304
	"""
	if "w3.org" in t.name:
		t.name = t.name.split("}")[1]
	if isinstance(t, XsdAtomicBuiltin):
		t.cpp_type = atomic_builtins[t.name]
	elif isinstance(t, XsdAtomicRestriction):
		anno_restriction(t)
	# Just read xs:lists into a string for now.
	# That simplifies validation and keeps heap allocation to nodes only.
	# VPR just reads list types into a string, too.
	elif isinstance(t, XsdList):
		t.cpp_type = "const char *"
	elif isinstance(t, XsdUnion):
		anno_union(t)
	else:
		raise NotImplementedError("I don't know what to do with type %s." % t)

def anno_complex_type(t: XsdComplexType) -> None:
	if getattr(t, "cpp_type", None) is not None: return
	t.cpp_type = to_cpp_type(t.name)

	# https://stackoverflow.com/a/39835527
	t.attribute_list = list(dict.fromkeys(t.attributes.values()))
	for attr in t.attribute_list:
		assert attr.use != "prohibited"
		anno_simple_type(attr.type)

	t.model = None
	t.child_elements = []
	if isinstance(t.content_type, XsdGroup) and len(t.content_type._group) > 0:
		if t.content_type.model == "all":
			t.model = "all"
		elif t.content_type.model in ["choice", "sequence"]:
			t.model = "dfa"
			t.dfa = dfa_from_group(t.content_type)
		else:
			raise NotImplementedError("Model group %s is not supported." % t.content_type.model)
		t.child_elements = anno_group(t.content_type)
	if t.has_simple_content():
		anno_simple_type(t.content_type)

for t in types:
	anno_complex_type(t)

for el in root_elements:
	anno_element(el)

# Sort types by tree height.
def key_ctype(x: XsdComplexType, visited=None):
	if not visited: visited=set()
	if x in visited: return 0
	else: visited.add(x)
	return max([0] + [key_ctype(y.type, visited) for y in getattr(x, "child_elements", [])]) + 1

types += anonymous_complex_types
types.sort(key=key_ctype)

#

def typedefn_from_union(t: XsdUnion) -> str:
	"""Generate a C++ declaration of a type-tagged union.

	struct union_foo {
		type_tag tag;
		union {
			double as_double;
			int as_int;
		}
	}
	type_tag is a special enum which is generated from the sum of the
	simple types seen so far in unions.
	"""
	out = "struct %s {\n" % t.cpp_type
	out += "\ttype_tag tag;\n"
	out += "\tunion {\n"
	for e in t.member_types:
		out += "\t\t%s %s;\n" % (e.cpp_type, to_union_member_type(e.cpp_type))
	out += "\t};\n"
	out += "};\n"
	return out

def typedefn_from_complex_type(t: XsdComplexType) -> str:
	"""Generate a C++ declaration of a struct, corresponding to a XsdComplexType.

	struct t_foo {
		int bar;
		bool has_my_baz;
		t_bar my_baz;
		collapsed_vec<t_quux, quux_pool> quuxes;
	}
	"""
	out = ""
	for attr in t.attribute_list:
		out += "%s %s;\n" % (attr.type.cpp_type, checked(attr.name))
	for child in t.child_elements:
		if child.many:
			out += "collapsed_vec<%s, %s_pool> %s;\n" % (child.cpp_type, child.type.name, pluralize(child.name))
		else:
			if child.optional:
				out += "bool has_%s;\n" % child.name
			out += "%s %s;\n" % (child.cpp_type, checked(child.name))
	if t.has_simple_content():
		out += "%s value;\n" % (t.content_type.cpp_type)

	out = "struct %s {\n" % t.cpp_type + indent(out) + "};\n"
	return out

simple_types = list(dict.fromkeys(simple_types))
type_tag_definition = "enum class type_tag {%s};\n" % ", ".join([to_token(t.cpp_type) for t in simple_types])

unions = list(dict.fromkeys(unions))
union_definitions = ""
for u in unions:
	struct_declarations += "struct %s;\n" % u.cpp_type
	union_definitions += typedefn_from_union(u) + "\n"

struct_declarations = ""
struct_definitions = ""
for t in types:
	struct_declarations += "struct %s;\n" % t.cpp_type
	struct_definitions += typedefn_from_complex_type(t)

root_element_declarations = ""
for el in root_elements:
	root_element_declarations += "class %s : public %s {\n" % (el.name, el.cpp_type)
	root_element_declarations += "public:\n"
	root_element_declarations += "\tpugi::xml_parse_result load(std::istream &is);\n"
	root_element_declarations += "\tvoid write(std::ostream &os);\n"
	root_element_declarations += "};\n"

count_fn_declarations = ""
for t in types:
	count_fn_declarations += "void count_%s(const pugi::xml_node &root);\n" % t.name

load_fn_declarations = ""
for t in types:
	load_fn_declarations += "void load_%s(const pugi::xml_node &root, %s *out);\n" % (t.name, t.cpp_type)

#

pool_types = list(dict.fromkeys(pool_types))
pool_declarations = ""
extern_pool_declarations = ""
for t in pool_types:
	extern_pool_declarations += "extern std::vector<%s> %s_pool;\n" % (t.cpp_type, t.name)
	pool_declarations += "std::vector<%s> %s_pool;\n" % (t.cpp_type, t.name)

clear_pools_declaration = "void clear_pools(void);\n"
clear_pools_definition = "void clear_pools(void){\n"
for t in pool_types:
	clear_pools_definition += "\t%s_pool.clear();\n" % t.name
clear_pools_definition += "}\n"

#

def tokens_from_enum(t: XsdAtomicRestriction) -> str:
	"""Generate C++ enum of token values from an XsdAtomicRestriction."""
	out = ""
	enum_tokens = ["UXSD_INVALID = 0"]
	enum_tokens += [to_token(x) for x in t.validators[0].enumeration]
	out += "enum class %s {%s};\n" % (t.cpp_type, ", ".join(enum_tokens))
	return out

def lookup_from_enum(t: XsdAtomicRestriction) -> str:
	"""Generate C++ lookup table of tokens to strings from an XsdAtomicRestriction."""
	out = ""
	lookup_tokens = ["\"UXSD_INVALID\""]
	lookup_tokens += ["\"%s\"" % x for x in t.validators[0].enumeration]
	out += "const char *lookup_%s[] = {%s};\n" % (t.name, ", ".join(lookup_tokens))
	return out

def lexer_from_enum(t: XsdAtomicRestriction) -> str:
	"""Generate a C++ function to convert const char *s to enum values generated
	from an XsdAtomicRestriction.

	It's in the form of enum_foo lex_foo(const char *in, bool throw_on_invalid)
	and currently uses a trie to parse the string.
	throw_on_invalid is a hacky parameter to determine if we should throw on
	an invalid value. It's currently necessary to read unions - we don't need to
	throw on an invalid value if we are trying to read into an union but we need
	to throw otherwise.
	"""
	assert t.cpp_type.startswith("enum")
	out = ""
	out += "inline %s lex_%s(const char *in, bool throw_on_invalid){\n" % (t.cpp_type, t.name)
	triehash_alph = [(x, "%s::%s" % (t.cpp_type, to_token(x))) for x in t.validators[0].enumeration]
	out += indent(triehash.gen_lexer_body(triehash_alph))
	out += "\tif(throw_on_invalid)\n"
	out += "\t\tthrow std::runtime_error(\"Found unrecognized enum value \" + std::string(in) + \" of %s.\");\n" % t.cpp_type
	out += "\treturn %s::UXSD_INVALID;\n" % t.cpp_type
	out += "}\n"
	return out

def tokens_from_complex_type(t: XsdComplexType) -> str:
	"""Generate one or two C++ enums of token values from an XsdComplexType.
	One enum is generated from valid attribute names and the other from child element names.
	"""
	out = ""
	if t.child_elements:
		enum_tokens = [to_token(e.name) for e in t.child_elements]
		lookup_tokens = ["\"%s\"" % e.name for e in t.child_elements]
		out += "enum class gtok_%s {%s};\n" % (t.cpp_type, ", ".join(enum_tokens))
		out += "const char *gtok_lookup_%s[] = {%s};\n" % (t.cpp_type, ", ".join(lookup_tokens))
	if t.attributes:
		enum_tokens = [to_token(x.name) for x in t.attribute_list]
		lookup_tokens = ["\"%s\"" % x.name for x in t.attribute_list]
		out += "enum class atok_%s {%s};\n" % (t.cpp_type, ", ".join(enum_tokens))
		out += "const char *atok_lookup_%s[] = {%s};\n" % (t.cpp_type, ", ".join(lookup_tokens))
	return out

def lexer_from_complex_type(t: XsdComplexType) -> str:
	"""Generate one or two C++ functions to convert const char *s to enum values
	generated from an XsdComplexType.

	It's in the form of (a|g)tok_foo (a|g)lex_foo(const char *in) and currently uses
	a trie to parse the string. a or g indicates if the token is an attribute token or a group
	(child element) token.
	"""
	out = ""
	if t.child_elements:
		out += "inline gtok_%s glex_%s(const char *in){\n" % (t.cpp_type, t.cpp_type)
		triehash_alph = [(e.name, "gtok_%s::%s" % (t.cpp_type, to_token(e.name))) for e in t.child_elements]
		out += indent(triehash.gen_lexer_body(triehash_alph))
		out += "\tthrow std::runtime_error(\"Found unrecognized child \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	if t.attributes:
		out += "inline atok_%s alex_%s(const char *in){\n" % (t.cpp_type, t.cpp_type)
		triehash_alph = [(x.name, "atok_%s::%s" % (t.cpp_type, to_token(x.name))) for x in t.attribute_list]
		out += indent(triehash.gen_lexer_body(triehash_alph))
		out += "\tthrow std::runtime_error(\"Found unrecognized attribute \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	return out

enums = list(dict.fromkeys(enums))

enum_tokens = ""
for e in enums:
	enum_tokens += tokens_from_enum(e)

enum_lookups = ""
for e in enums:
	enum_lookups += lookup_from_enum(e)

enum_lexers = ""
for e in enums:
	enum_lexers += lexer_from_enum(e) + "\n"

complex_type_tokens = ""
for t in types:
	complex_type_tokens += tokens_from_complex_type(t)

complex_type_lexers = ""
for t in types:
	complex_type_lexers += lexer_from_complex_type(t) + "\n"

#

def _gen_dfa_table(t: XsdGroup) -> str:
	"""Generate a 2D C++ array representing DFA table from an XsdGroup.dfa.

	"dfa" property is generated and assigned to XsdGroups in anno_type_element.
	The array is indexed by the state and input token value, such that table[state][input]
	gives the next state.
	"""
	out = ""
	out += "int gstate_%s[%d][%d] = {\n" % (t.cpp_type, len(t.dfa.states), len(t.dfa.alphabet))
	for i in range(0, max(t.dfa.states)+1):
		state = t.dfa.transitions[i]
		row = [str(state[x]) if state.get(x) is not None else "-1" for x in t.dfa.alphabet]
		out += "\t{%s},\n" % ", ".join(row)
	out += "};\n"
	return out

# TODO: Find a cleaner way to load unions.
def _gen_load_union(t: XsdUnion, container: str, input: str) -> str:
	out = ""
	for m in t.member_types:
		new_container = "%s.%s" % (container, to_union_member_type(m.cpp_type))
		out += "%s.tag = type_tag::%s;\n" % (container, to_token(m.cpp_type))
		if isinstance(m, XsdAtomicBuiltin):
			out += "%s = %s;\n" % (new_container, atomic_builtin_load_formats[m.name] % input)
			out += "if(errno == 0)\n"
			out += "\tbreak;\n"
		elif isinstance(m, XsdAtomicRestriction):
			out += "%s = lex_%s(%s, false);\n" % (new_container, m.name, input)
			out += "if(%s != %s::UXSD_INVALID)\n" % (new_container, m.cpp_type)
			out += "break;\n"
		else:
			raise NotImplementedError("I don't know how to load %s into a union." % m)
	out += "throw std::runtime_error(\"Couldn't load a suitable value into union %s.\");\n" % t.name
	return out

# See https://stackoverflow.com/questions/26080829/detecting-strtol-failure
# Since detecting additional characters require some other hoops which would
# hurt performance, we only check errno.
def _gen_load_simple(t: XsdSimpleType, container: str, input: str) -> str:
	out = ""
	if isinstance(t, XsdAtomicBuiltin):
		out += "%s = %s;\n" % (container, atomic_builtin_load_formats[t.name] % input)
		out += "if(errno != 0)\n"
		out += "\tthrow std::runtime_error(\"Invalid value `\" + std::string(%s) + \"` to load a %s into %s.\");\n" % (input, t.cpp_type, container)
	elif isinstance(t, XsdAtomicRestriction):
		out += "%s = lex_%s(%s, true);\n" % (container, t.name, input)
	elif isinstance(t, XsdList):
		out += "%s = strdup(%s);\n" % (container, input)
	elif isinstance(t, XsdUnion):
		out += _gen_load_union(t, container, input)
	else:
		raise NotImplementedError("I don't know how to load %s." % t)
	return out

def _gen_load_element_complex(t: XsdElement, parent: str) -> str:
	out = ""
	if t.many:
		out += "%s->%s.push_back(%s());\n" % (parent, pluralize(t.name), t.cpp_type)
		out += "load_%s(node, &%s->%s.back());\n" % (t.type.name, parent, pluralize(t.name))
	else:
		out += "load_%s(node, &%s->%s);\n" % (t.type.name, parent, t.name)
		if t.optional:
			out += "%s->has_%s = 1;\n" % (parent, t.name)
	return out

def _gen_load_element_simple(t: XsdElement, parent: str) -> str:
	out = ""
	if t.many:
		container = "%s->%s" % (parent, pluralize(t.name))
		out += "%s.push_back(0);\n" % container
		out += _gen_load_simple(t.type, "%s.back()" % container, "node.child_value()")
	else:
		container = "%s->%s" % (parent, t.name)
		out += _gen_load_simple(t.type, container, "node.child_value()")
	return out

def _gen_load_element(t: XsdElement, parent: str) -> str:
	if isinstance(t.type, XsdComplexType):
		return _gen_load_element_complex(t, parent)
	else:
		return _gen_load_element_simple(t, parent)

def _gen_load_dfa(t: XsdGroup) -> str:
	"""Partial function to generate the child element validation&loading portion
	of a C++ function load_foo, if the model group is an xs:sequence or xs:choice.

	xs:sequence/xs:choice groups can be compiled down to a finite automaton.
	This is done in dfa.py. C++ state table is generated in _gen_dfa_table and the
	stream of child elements are validated according to the table here.

	The C++ table has -1s in place of invalid state transitions. If we step into a -1,
	we call dfa_error. We check again at the end of input. If we aren't in an accepted
	state, we again call dfa_error."""
	out = ""

	out += "int next, state=%d;\n" % t.dfa.start
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	out += "\tgtok_%s in = glex_%s(node.name());\n" % (t.cpp_type, t.cpp_type)

	out += "\tnext = gstate_%s[state][(int)in];\n" % t.cpp_type
	out += "\tif(next == -1)\n"
	out += "\t\tdfa_error(gtok_lookup_%s[(int)in], gstate_%s[state], gtok_lookup_%s, %d);\n"\
					% (t.cpp_type, t.cpp_type, t.cpp_type, len(t.dfa.alphabet))
	out += "\tstate = next;\n"

	out += "\tswitch(in){\n";
	for el in t.child_elements:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp_type, to_token(el.name))
		out += indent(_gen_load_element(el, "out"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";

	reject_cond = " && ".join(["state != %d" % x for x in t.dfa.accepts])
	out += "}\n"
	out += "if(%s) dfa_error(\"end of input\", gstate_%s[state], gtok_lookup_%s, %d);\n"\
			% (reject_cond, t.cpp_type, t.cpp_type, len(t.dfa.alphabet))

	return out

def _gen_load_all(t: XsdGroup) -> str:
	"""Partial function to generate the child element validation&loading portion
	of a C++ function load_foo, if the model group is an xs:all.

	xs:alls can be validated in a similar fashion to xs:attributes. We maintain a
	bitset of which elements are found. At the end, we OR our bitset with the value
	corresponding to the optional elements and check if all bits in it are set. If not,
	we call attr_error with the token lookup table and the OR'd bitset."""
	out = ""
	N = len(t.child_elements)

	out += "std::bitset<%d> gstate = 0;\n" % N
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	out += "\tgtok_%s in = glex_%s(node.name());\n" % (t.cpp_type, t.cpp_type)

	out += "\tif(gstate[(int)in] == 0) gstate[(int)in] = 1;\n"
	out += "\telse throw std::runtime_error(\"Duplicate element \" + std::string(node.name()) + \" in <%s>.\");\n" % t.name

	out += "\tswitch(in){\n";
	for el in t.child_elements:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp_type, to_token(el.name))
		out += indent(_gen_load_element(el, "out"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.occurs[0] == 0 else "0" for x in t.child_elements][::-1])
	out += "std::bitset<%d> test_gstate = gstate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_gstate.all()) all_error(test_gstate, gtok_lookup_%s);\n" % t.cpp_type

	return out

def _gen_load_attrs(t: XsdGroup) -> str:
	"""Partial function to generate the attribute loading portion of a C++
	function load_foo. See _gen_load_all to see how attributes are validated."""
	out = ""
	N = len(t.attribute_list)
	out += "std::bitset<%d> astate = 0;\n" % N
	out += "for(pugi::xml_attribute attr = root.first_attribute(); attr; attr = attr.next_attribute()){\n"
	out += "\tatok_%s in = alex_%s(attr.name());\n" % (t.cpp_type, t.cpp_type)
	out += "\tif(astate[(int)in] == 0) astate[(int)in] = 1;\n"
	out += "\telse throw std::runtime_error(\"Duplicate attribute \" + std::string(attr.name()) + \" in <%s>.\");\n" % t.name

	out += "\tswitch(in){\n";
	for attr in t.attribute_list:
		out += "\tcase atok_%s::%s:\n" % (t.cpp_type, to_token(attr.name))
		out += indent(_gen_load_simple(attr.type, "out->%s" % checked(attr.name), "attr.value()"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.use == "optional" else "0" for x in t.attribute_list][::-1])
	out += "std::bitset<%d> test_astate = astate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_astate.all()) attr_error(test_astate, atok_lookup_%s);\n" % t.cpp_type
	return out

def load_fn_from_complex_type(t: XsdComplexType) -> str:
	"""Generate a full C++ function load_foo(&root, *out)
	which can load an XSD complex type from DOM &root into C++ type *out."""
	out = ""

	out += "void load_%s(const pugi::xml_node &root, %s *out){\n" % (t.name, t.cpp_type)
	if t.model == "dfa":
		out = _gen_dfa_table(t) + out
		out += indent(_gen_load_dfa(t))
	elif t.model == "all":
		out += indent(_gen_load_all(t))
	elif t.has_simple_content():
		out += indent(_gen_load_simple(t.content_type, "out->value", "root.child_value()"))
	else:
		out += "\tif(root.first_child().type() == pugi::node_element)\n"
		out += "\t\tthrow std::runtime_error(\"Unexpected child element in <%s>.\");\n" % t.name

	out += "\n"
	if t.attributes:
		out += indent(_gen_load_attrs(t))
	else:
		out += "\tif(root.first_attribute())\n"
		out += "\t\tthrow std::runtime_error(\"Unexpected attribute in <%s>.\");\n" % t.name

	out += "}\n"
	return out

load_fn_definitions = "\n".join([load_fn_from_complex_type(t) for t in types])

#

def load_fn_from_root_element(el: XsdElement) -> str:
	out = ""
	out += "pugi::xml_parse_result %s::load(std::istream &is){\n" % checked(el.name)
	out += "\tpugi::xml_document doc;\n"
	out += "\tpugi::xml_parse_result result = doc.load(is);\n"
	out += "\tif(!result) return result;\n"
	out += "\tfor(pugi::xml_node node= doc.first_child(); node; node = node.next_sibling()){\n"

	out += "\t\tif(std::strcmp(node.name(), \"%s\") == 0){\n" % el.name
	out += "\t\t\t/* If errno is set up to this point, it messes with strtol errno checking. */\n"
	out += "\t\t\terrno = 0;\n"
	out += "\t\t\tload_%s(node, this);\n" % el.name

	out += "\t\t}\n"
	out += "\t\telse throw std::runtime_error(\"Invalid root-level element \" + std::string(node.name()));\n"
	out += "\t}\n"
	out += "\treturn result;\n"
	out += "}\n"
	return out

root_element_definitions = ""
for el in root_elements:
	root_element_definitions += load_fn_from_root_element(el) + "\n"

#

def _gen_write_simple(t: XsdSimpleType, container: str, attr_name: str="") -> str:
	"""Partial function to generate code which writes out a simple type.

	The attr_name parameter is passed by _gen_write_attr so that we can
	generate squashed code like `os << "index=\"" << y_list.index << "\"";`.
	"""
	out = ""
	if isinstance(t, XsdAtomicBuiltin) or isinstance(t, XsdList):
		if attr_name:
			out += "os << \" %s=\\\"\" << %s << \"\\\"\";\n" % (attr_name, container)
		else:
			out += "os << %s;\n" % container
	elif isinstance(t, XsdAtomicRestriction):
		if attr_name:
			out += "os << \" %s=\\\"\" << lookup_%s[(int)%s] << \"\\\"\";\n" % (attr_name, t.name, container)
		else:
			out += "os << lookup_%s[(int)%s];\n" % (t.name, container)
	elif isinstance(t, XsdUnion):
		for m in t.member_types:
			out += "if(%s.tag == type_tag::%s)" % (container, to_token(m.cpp_type))
			out += indent(_gen_write_simple(t, container + "." + to_union_member_type(m.cpp_type), attr_name))
	else:
		raise NotImplementedError("I don't know how to write out %s." % t)
	return out

def _gen_write_attr(a: XsdAttribute, container: str) -> str:
	"""Partial function to generate code which writes out a single XML attribute."""
	out = ""
	new_container = "%s.%s" % (container, a.name)
	if a.use == "required" or a.default:
		out += _gen_write_simple(a.type, new_container, a.name)
	else:
		out += "if((bool)%s)\n" % new_container
		out += indent(_gen_write_simple(a.type, new_container, a.name))
	return out

def _gen_write_element(el: XsdElement, container: str) -> str:
	"""Partial function to generate C++ code for writing out a struct generated
	from an XsdElement as XML.

	Currently, all values with non-zero default values are emitted anyway.
	Otherwise, we would have to check against the nonzero value, and the
	check would create a case split for all simple types again.(how to compare
	unions? strings? doubles?)
	"""
	out = ""
	if isinstance(el.type, XsdSimpleType):
		out += "os << \"<%s>\";\n" % el.name
		out += _gen_write_simple(el.type, container)
		out += "os << \"</%s>\";\n" % el.name
	else:
		if el.type.attribute_list:
			out += "os << \"<%s\";\n" % el.name
			for a in el.type.attribute_list:
				out += _gen_write_attr(a, container)
			out += "os << \">\";\n"
		else:
			out += "os << \"<%s>\";\n" % el.name

		for e in el.type.child_elements:
			if e.many:
				out += "for(auto &%s: %s.%s){\n" % (checked(e.name), container, pluralize(e.name))
				out += indent(_gen_write_element(e, checked(e.name)))
				out += "}\n"
			else:
				new_container = "%s.%s" % (container, e.name)
				if e.optional:
					out += "if(%s.has_%s){\n" % (container, e.name)
					out += indent(_gen_write_element(e, new_container))
					out += "}\n"
				else:
					out += _gen_write_element(e, new_container)
		if el.type.has_simple_content():
			out += _gen_write_simple(el.type.content_type, el.name+".value")
		out += "os << \"</%s>\";\n" % el.name
	return out

def write_fn_from_element(t: XsdElement) -> str:
	out = ""
	out += "void %s::write(std::ostream &os){\n" % checked(t.name)
	out += "\t/* Print floating points with max double precision. */\n"
	out += "\tos.precision(std::numeric_limits<double>::max_digits10);\n"
	out += indent(_gen_write_element(t, "(*this)"))
	out += "}\n"
	return out

for el in root_elements:
	root_element_definitions += write_fn_from_element(el) + "\n"

#

# Don't emit functions for these if not needed.
has_dfa = bool([x for x in types if x.model == "dfa"])
has_attr = bool([x for x in types if x.attributes])
has_all = bool([x for x in types if x.model == "all"])
has_pool = bool(pool_types)

# Template-ish for header file. We use a bunch of file.writes and f-strings here.
# Jinja2 is really not worth the trouble but we do want some control over our
# template - don't write one more newline if we have no pools.

header_file = open(header_file_name, "w")

header_file.write(f"""#include <bitset>
#include <cassert>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include <error.h>
#include <stddef.h>
#include <stdint.h>
#include "pugixml.hpp"

/*
 * This file is generated by uxsdcxx.
 * Modify only if your build process doesn't involve regenerating this file.
 *
 * Cmdline: { " ".join(sys.argv) }
 * Input file: { file_name }
 * md5sum of input file: { md5(file_name) }
 */

/* All uxsdcxx functions and structs live in this namespace. */
namespace uxsd {{

{ triehash.gen_prelude() }
""")

header_file.write("""
/* Stores a vector of elements in a shared pool. Consists
 * of an offset into the pool and a size. It's faster than a regular
 * vector, but one can only insert into it when it's the last vector
 * in the pool. */
template<class T, std::vector<T> &pool>
class collapsed_vec {
public:
	uint32_t size;
	uint32_t offset;
	collapsed_vec(){
		size = 0;
		offset = pool.size();
	}
	T& back(){
		return pool[offset+size-1];
	}
	T* begin(){
		return &pool[offset];
	}
	T* end(){
		return &pool[offset+size];
	}
	T& operator[](uint32_t i){
		return pool[offset+i];
	}
	void push_back(const T &x){
		assert(size+offset == pool.size());
		pool.push_back(x);
		size++;
	}
};
""")

header_file.write("/* Forward declaration of generated data types. Needed for the pools. */\n")
if struct_declarations: header_file.write(struct_declarations+"\n")

header_file.write("/* Global shared pools for storing multiply-occurring elements. */\n")
header_file.write(extern_pool_declarations+"\n\n")

header_file.write("/* Helper function for freeing the pools. */\n")
header_file.write(clear_pools_declaration+"\n")

header_file.write("/* Data type definitions generated from the XSD schema. */\n")
if enum_tokens: header_file.write(enum_tokens+"\n")
if simple_types: header_file.write(type_tag_definition+"\n")
if union_definitions: header_file.write(union_definitions+"\n")
if struct_definitions: header_file.write(struct_definitions+"\n")
header_file.write(root_element_declarations+"\n")

header_file.write("/* Loading functions. They validate the DOM data\n")
header_file.write(" * and load it into the generated structures. */\n")
header_file.write(load_fn_declarations+"\n")

if has_dfa:
	header_file.write("void dfa_error(const char *wrong, int *states, const char **lookup, int len);\n")
if has_all:
	header_file.write("template<std::size_t N>\n")
	header_file.write("void all_error(std::bitset<N> gstate, const char **lookup);\n")
if has_attr:
	header_file.write("template<std::size_t N>\n")
	header_file.write("void attr_error(std::bitset<N> astate, const char **lookup);\n")

header_file.write("} /* namespace uxsd */\n")

header_file.close()

#
# Template-ish for implementation file.

impl_file = open(impl_file_name, "w")

impl_file.write(f"""#include "{ header_file_name }"

/*
 * This file is generated by uxsdcxx.
 * Modify only if your build process doesn't involve regenerating this file.
 *
 * Cmdline: { " ".join(sys.argv) }
 * Input file: { file_name }
 * md5sum of input file: { md5(file_name) }
 */

/* All uxsdcxx functions and structs live in this namespace. */
namespace uxsd {{

{ pool_declarations }

{ clear_pools_definition }

/* Lookup tables for enums. */
{ enum_lookups }

{ root_element_definitions }

/* Tokens for attribute and node names. */
{ complex_type_tokens }

/* Lexing functions. These convert the const char *s of PugiXML to enums.
 * You may find numerous \"break\"s there. Without them, a warning explosion ensues. */
{ enum_lexers }
{ complex_type_lexers }

/* Loading functions. They validate the DOM data and load it into the generated structures. */
{ load_fn_definitions }
""")

if has_dfa:
	impl_file.write("""
void dfa_error(const char *wrong, int *states, const char **lookup, int len){
	std::vector<std::string> expected;
	for(int i=0; i<len; i++){
		if(states[i] != -1) expected.push_back(lookup[i]);
	}

	std::string expected_or = expected[0];
	for(unsigned int i=1; i<expected.size(); i++)
		expected_or += std::string(" or ") + expected[i];

	throw std::runtime_error("Expected " + expected_or + ", found " + std::string(wrong));
}
""")

if has_attr:
	impl_file.write("""
template<std::size_t N>
void attr_error(std::bitset<N> astate, const char **lookup){
	std::vector<std::string> missing;
	for(unsigned int i=0; i<N; i++){
		if(astate[i] == 0) missing.push_back(lookup[i]);
	}

	std::string missing_and = missing[0];
	for(unsigned int i=1; i<missing.size(); i++)
		missing_and += std::string(", ") + missing[i];

	throw std::runtime_error("Didn't find required attributes " + missing_and + ".");
}
""")

if has_all:
	impl_file.write("""
/* runtime error for <xs:all>s */
template<std::size_t N>
void all_error(std::bitset<N> gstate, const char **lookup){
	std::vector<std::string> missing;
	for(unsigned int i=0; i<N; i++){
		if(gstate[i] == 0) missing.push_back(lookup[i]);
	}

	std::string missing_and = missing[0];
	for(unsigned int i=1; i<missing.size(); i++)
		missing_and += std::string(", ") + missing[i];

	throw std::runtime_error("Didn't find required elements " + missing_and + ".");
}
""")

impl_file.write("} /* namespace uxsd */\n")
impl_file.close()
