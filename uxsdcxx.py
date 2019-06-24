# Script to convert an XSD schema to a C++ struct definition and
# a parser based on PugiXML.

from pprint import pprint

import copy
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

# Note that Python type annotations are just annotations and
# have no runtime effects. But they are useful for documentation.
from typing import List, Tuple, Dict, Set, Union

from dfa import dfa_from_group

xs = "{http://www.w3.org/2001/XMLSchema}"
atomic_builtins = {
	xs+"string": "std::string",
	xs+"boolean": "bool",
	xs+"float": "float",
	xs+"decimal": "int",
	xs+"integer": "int",
	xs+"nonPositiveInteger": "int",
	xs+"negativeInteger": "int",
	xs+"long": "long",
	xs+"int": "int",
	xs+"short": "short",
	xs+"byte": "char",
	xs+"nonNegativeInteger": "unsigned int",
	xs+"unsignedLong": "unsigned long",
	xs+"unsignedInt": "unsigned int",
	xs+"unsignedShort": "unsigned short",
	xs+"unsignedByte": "unsigned byte",
	xs+"positiveInteger": "unsigned int",
	xs+"double": "double",
}

# TODO: Make actual validators for these.
atomic_builtin_load_formats = {
	xs+"string": "%s",
	xs+"boolean": "std::stoi(%s)",
	xs+"float": "std::stof(%s)",
	xs+"decimal": "std::stoi(%s)",
	xs+"integer": "std::stoi(%s)",
	xs+"nonPositiveInteger": "std::stoi(%s)",
	xs+"negativeInteger": "std:.stoi(%s)",
	xs+"long": "std::stol(%s)",
	xs+"int": "std::stoi(%s)",
	xs+"short": "std::stoi(%s)",
	xs+"byte": "std::stoi(%s)",
	xs+"nonNegativeInteger": "std::stoul(%s)",
	xs+"unsignedLong": "std::stoul(%s)",
	xs+"unsignedInt": "std::stoul(%s)",
	xs+"unsignedShort": "std::stoul(%s)",
	xs+"unsignedByte": "std::stoul(%s)",
	xs+"positiveInteger": "std::stoul(%s)",
	xs+"double": "std::stod(%s)",
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

schema = xmlschema.XMLSchema("fpga_architecture.xsd")

# Complex types found inside elements. They are not found in the global map,
# so we have to reserve them while traversing types in the global map
# and generate them afterwards.
anonymous_complex_types = set()

# Enumerations and unions found inside elements.
enums = set()
unions = set()

# Simple types found inside unions.
# We generate a special "types" enum from this, to put in all tagged union type definitions.
simple_types = set()

# Get all global user-defined types.
types: List[XsdComplexType] = [v for k, v in schema.types.items() if "w3.org" not in k and isinstance(v, XsdComplexType)]
elements: List[XsdElement] = [v for v in schema.elements.values()]

# Check against keywords, warn and rename if necessary.
def _(x: str) -> str:
	if x in cpp_keywords:
		print("%s is a C++ keyword. Changing it to %s_." % (x, x), file=sys.stderr)
		return x + "_"
	return x

# Convert type name to C++ type. Elements can have builtins as types.
def to_cpp_type(x: str) -> str:
	if x in atomic_builtins: return atomic_builtins[x]
	else: return "t_%s" % x

def to_enum_token(x: str) -> str:
	return re.sub(r"[^a-zA-Z0-9_]", "_", x).upper()

def to_union_member_type(x: str) -> str:
	return "as_%s" % re.sub(r"[^a-zA-Z0-9_]", "_", x)

def indent(x: str, n: int=1) -> str:
	return "\n".join(["\t"*n + line for line in x.split("\n") if line])

# Return count of child types. This is used to lower single-element structs
# to typedefs and lower unique_ptrs to typedefs to flat members.
def count_child_types(t, visited=None) -> int:
	if visited is None: visited = {} # A wart of Python. https://docs.python-guide.org/writing/gotchas/
	if visited.get(t): return 0
	visited[t] = True
	out = 0
	if isinstance(t, XsdComplexType):
		out += len(t.attributes)
		out += count_child_types(t.content_type, visited)
	elif isinstance(t, XsdGroup):
		for e in t._group:
			out += count_child_types(e, visited)
	elif isinstance(t, XsdSimpleType) or isinstance(t, XsdElement):
		out = 1
	else:
		raise NotImplementedError("I don't know how to count %s." % t)
	return out

#

# We can put a "cpp_name" annotation on elements because they have actual names.
# In comparison, complex_types only have type names. A complex_type could have
# different names if put in different structs.
def anno_type_element(t: XsdElement, visited=None, many=False):
	if visited is None: visited = {}
	if visited.get(t): return
	visited[t] = True

	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	if not t.type.name:
		t.type.name = t.name
		anonymous_complex_types.add(t.type)

	if isinstance(t.type, XsdComplexType):
		anno_type_complex_type(t.type, visited)
		# We have a recursive type and anno_typedecl_complex_type did nothing.
		if not getattr(t.type, "cpp_type", None): return
	else:
		anno_type_simple_type(t.type)

	if many:
		t.cpp_type = "std::vector<%s>" % t.type.cpp_type
		t.cpp_name = "%s_list" % t.name
	else:
		t.cpp_type = "std::unique_ptr<%s>" % t.type.cpp_type
		t.cpp_name = "%s" % _(t.name)

def anno_type_group(t: XsdGroup, visited=None, many=False):
	if visited is None: visited = {}
	if visited.get(t): return
	visited[t] = True

	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	for e in t._group:
		if isinstance(e, XsdGroup):
			anno_type_group(e, visited, many)
		elif isinstance(e, XsdElement):
			anno_type_element(e, visited, many)
		else:
			raise NotImplementedError("I don't know what to do with group member %s." % e)

# Only enumerations are supported as restrictions.
def anno_type_restriction(t: XsdAtomicRestriction):
	assert len(t.validators) == 1, "I can only handle simple enumerations."
	if t.name:
		t.cpp_type = "enum_%s" % t.name
	else:
		# Possibly member of an XsdList or XsdUnion.
		t.cpp_type = "enum_%s" % t.parent.name
	enums.add(t)

def anno_type_union(t: XsdUnion):
	t.cpp_type = "union_%s" % t.name
	for m in t.member_types:
		anno_type_simple_type(m)
		simple_types.add(m)
	unions.add(t)

# See https://www.obj-sys.com/docs/xbv23/CCppUsersGuide/ch04.html.
def anno_type_simple_type(t: XsdSimpleType):
	if isinstance(t, XsdAtomicBuiltin):
		t.cpp_type = atomic_builtins[t.name]
	elif isinstance(t, XsdList):
		t.cpp_base_type = atomic_builtins[t.base_type.name]
		t.cpp_type = "std::vector<%s>" % t.cpp_base_type
	elif isinstance(t, XsdAtomicRestriction):
		anno_type_restriction(t)
	elif isinstance(t, XsdUnion):
		anno_type_union(t)
	else:
		raise NotImplementedError("I don't know what to do with type %s." % t)

def anno_type_complex_type(t: XsdComplexType, visited=None):
	if visited is None: visited = {}
	if visited.get(t): return
	visited[t] = True
	for attr in t.attributes.values():
		anno_type_simple_type(attr.type)
	if isinstance(t.content_type, XsdGroup):
		if len(t.content_type._group) > 0:
			anno_type_group(t.content_type, visited)
	else:
		anno_type_simple_type(t.content_type)
	t.cpp_type = to_cpp_type(t.name)

#

# Generate a tagged union type.
# "types" is a reserved enum which is generated from the atomic builtins.
def typedefn_from_union(t: XsdUnion) -> str:
	out = "struct %s {\n" % t.cpp_type
	out += "\ttype_tag tag;\n"
	out += "\tunion {\n"
	for e in t.member_types:
		out += "\t\t%s %s;\n" % (e.cpp_type, to_union_member_type(e.cpp_type))
	out += "\t};\n"
	out += "};\n"
	return out

def typedefn_from_enum(t: XsdAtomicRestriction) -> str:
	enum_values = [to_enum_token(x) for x in t.validators[0].enumeration]
	return "enum class %s {%s};" % (t.cpp_type, ", ".join(enum_values))

def _typedefn_from_group(t: XsdGroup) -> str:
	out = ""
	for e in t._group:
		if isinstance(e, XsdGroup):
			out += _typedefn_from_group(e) + "\n"
		elif isinstance(e, XsdElement):
			out += "%s %s;\n" % (e.cpp_type, e.cpp_name)
		else:
			raise NotImplementedError("I don't know what to do with group member %s." % e)
	return out[:-1]

def typedefn_from_complex_type(t: XsdComplexType) -> str:
	out = ""
	for attr in t.attributes.values():
		out += "%s %s;\n" % (attr.type.cpp_type, _(attr.name))
	if isinstance(t.content_type, XsdGroup):
		if len(t.content_type._group) > 0:
			out += _typedefn_from_group(t.content_type) + "\n"
	else:
		out += "%s value;\n" % t.content_type.cpp_type

	out = "struct %s {\n" % t.cpp_type + indent(out) + "\n};\n"
	return out

# Annotate the schema with cpp_types of every complex and simple type.
# Put aside anonymous complex types, enums and unions for generating actual type declaration.
for t in types:
	anno_type_complex_type(t)

for e in elements:
	anno_type_element(e)

types += anonymous_complex_types

# Generate type declarations and definitions, and function declarations.

struct_declarations = ""
struct_definitions = ""
for t in types:
	struct_declarations += "struct %s;\n" % t.cpp_type
	struct_definitions += typedefn_from_complex_type(t)

enum_definitions = ""
for e in enums:
	enum_definitions += typedefn_from_enum(e) + "\n"

union_definitions = ""
for u in unions:
	struct_declarations += "struct %s;\n" % u.cpp_type
	union_definitions += typedefn_from_union(u) + "\n"

element_definitions = ""
for e in elements:
	element_definitions += "%s %s;\n" % (e.cpp_type, e.cpp_name)

# Put the types found so far in unions in an enum. This is our tag enum.
type_tag_definition = "enum class type_tag {%s};\n" % ", ".join(set([to_enum_token(t.cpp_type) for t in simple_types]))

fn_declarations = ""
for t in types:
	fn_declarations += "void read_%s(pugi::xml_node &root, %s &out);\n" % (t.cpp_type, t.cpp_type)

print("#include <cstring>")
print("#include <memory>")
print("#include <string>")
print("#include <vector>")
print("")
print("#include \"pugixml.hpp\"")
print("")
if struct_declarations: print(struct_declarations)
if enum_definitions: print(enum_definitions)
if simple_types: print(type_tag_definition)
if union_definitions: print(union_definitions)
if struct_definitions: print(struct_definitions)
if element_definitions: print(element_definitions)
if fn_declarations: print(fn_declarations, end="")

#

def _gen_load_simple(t: XsdSimpleType, container: str, input: str="node.child_value()") -> str:
	out = ""
	if t.cpp_type.startswith("std::vector"):
		out += "std::string raw, word;\n"
		out += "raw = %s;\n" % input
		out += "while(raw >> word){\n" % input
		out += "\t%s.push_back(%s);\n" % (container, atomic_builtin_load_formats[t.base_type.name] % "word")
		out += "}\n"
	else:
		out += "%s = %s;\n" % (container, atomic_builtin_load_formats[t.name] % input)
	return out

def _gen_load_element_complex(t: XsdElement, parent: str="") -> str:
	out = ""
	container = "%s%s" % ("%s." % parent if parent else "", t.cpp_name)
	if t.cpp_type.startswith("std::vector"):
		out += "%s tmp;\n" % t.type.cpp_type
		out += "%s.push_back(tmp);\n" % container
		out += "read_%s(node, %s.back());\n" % (t.type.cpp_type, container)
	elif t.cpp_type.startswith("std::unique_ptr"):
		out += "%s = %s(new %s);\n" % (container, t.cpp_type, t.type.cpp_type)
		out += "read_%s(node, *%s);\n" % (t.type.cpp_type, container)
	else:
		raise ValueError("It should be impossible for an Element to have type %s." % t.cpp_type)
	return out

def _gen_load_element(t: XsdElement, parent: str="") -> str:
	if isinstance(t.type, XsdComplexType):
		return _gen_load_element_complex(t, parent)
	elif isinstance(t.type, XsdAtomicBuiltin):
		container = "%s%s" % ("%s." % parent if parent else "", t.cpp_name)
		if t.cpp_type.startswith("std::vector"):
			return "%s.push_back(%s);\n" % (container, atomic_builtin_load_formats[t.type.name] % "node.child_value()")
		return "%s = %s;\n" % (container, atomic_builtin_load_formats[t.type.name] % "node.child_value()")
	else:
		raise NotImplementedError("%s?" % t.type)

def gen_init_fn() -> str:
	out = """
void read(const char *filename){
	pugi::xml_document doc;
	pugi::xml_parse_result result = doc.load_file(filename);
	if(!result){
		throw std::runtime_error("Could not load XML file " + std::string(filename) + ".");
	}
	pugi::xml_node node = doc.first_child();
	for(; node; node = node.next_sibling()){
"""
	for i, e in enumerate(elements):
		out += "		%s(std::strcmp(node.name(), \"%s\") == 0){\n" % ("if" if i == 0 else "else if", e.name)
		out += indent(_gen_load_element(e), 3) + "\n"
		out += "		}\n"
	out += "		else throw std::runtime_error(\"Invalid root-level element \" + std::string(node.name()));\n"
	out += "	}\n"
	out += "}\n"
	return out

def _gen_load_group(t: XsdGroup) -> str:
	dfa = dfa_from_group(t)
	out = """
pugi::xml_node node = root.first_child();
const char *in = node.name();
int state = %s;
while(1){
	switch(state){
""" % dfa["start"]
	for s in dfa["states"]:
		out += "\tcase %d:\n" % s
		for i, (inp, next) in enumerate(dfa["transitions"][s].items()):
			out += "\t\t%s(strcmp(in, \"%s\") == 0){\n" % ("if" if i == 0 else "else if", inp)
			out += "\t\t\tstate = %d;\n" % next
			out += indent(_gen_load_element(dfa["elements"][inp], "out"), 3) + "\n"
			out += "\t\t}\n"
		if s in dfa["accepts"]:
			out += "\t\telse if(strcmp(in, \"end of input\") == 0){\n"
			out += "\t\t\tgoto accept;\n"
			out += "\t\t}\n"
		expected_inputs = ["<%s>" % x for x in dfa["transitions"][s].keys()]
		if s in dfa["accepts"]: expected_inputs.append("end of input")
		out += "\t\telse throw std::runtime_error(\"expected %s, found \" + std::string(in));\n" % " or ".join(expected_inputs)
		out += "\t\tbreak;\n"
		out += "\t}\n"

	out += "\tnode = node.next_sibling();\n"
	out += "\tif(node) in = node.name();\n"
	out += "\telse in = \"end of input\";\n"
	out += "}\n"
	return out

def gen_complex_type_fn(t: XsdComplexType) -> str:
	out = """
void read_%s(pugi::xml_node &root, %s &out){
""" % (t.cpp_type, t.cpp_type)
	if isinstance(t.content_type, XsdGroup) and t.content_type._group:
		out += indent(_gen_load_group(t.content_type))
	out += "accept:\n"
	out += "\treturn;\n"
	out += "}\n"
	return out

# Generate function definitions.

for t in types:
	print(gen_complex_type_fn(t))

print(gen_init_fn())
