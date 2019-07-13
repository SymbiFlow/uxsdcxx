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

from itertools import chain
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
	xs+"boolean": "std::strtol(%s, NULL, 10)",
	xs+"float": "std::strtof(%s, NULL)",
	xs+"decimal": "std::strtol(%s, NULL, 10)",
	xs+"integer": "std::strtol(%s, NULL, 10)",
	xs+"nonPositiveInteger": "std::strtol(%s, NULL, 10)",
	xs+"negativeInteger": "std::strtol(%s, NULL, 10)",
	xs+"long": "std::strtoll(%s, NULL, 10)",
	xs+"int": "std::strtol(%s, NULL, 10)",
	xs+"short": "std::strtol(%s, NULL, 10)",
	xs+"byte": "std::strtol(%s, NULL, 10)",
	xs+"nonNegativeInteger": "std::strtoul(%s, NULL, 10)",
	xs+"unsignedLong": "std::strtoull(%s, NULL, 10)",
	xs+"unsignedInt": "std::strtoul(%s, NULL, 10)",
	xs+"unsignedShort": "std::strtoul(%s, NULL, 10)",
	xs+"unsignedByte": "std::strtoul(%s, NULL, 10)",
	xs+"positiveInteger": "std::strtoul(%s, NULL, 10)",
	xs+"double": "std::strtod(%s, NULL)",
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

schema = xmlschema.validators.XMLSchema11("rr_graph.xsd")

# Complex types found inside ". They are not found in the global map,
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

#

# We can put a "cpp_name" annotation on elements because they have actual names.
# In comparison, complex_types only have type names. A complex_type could have
# different C++ names in different contexts.
def anno_type_element(t: XsdElement, many=False):
	if getattr(t, "cpp_type", None) is not None: return

	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	if not t.type.name:
		t.type.name = t.name
		anonymous_complex_types.add(t.type)

	if isinstance(t.type, XsdComplexType):
		anno_type_complex_type(t.type)
		# We have a recursive type and anno_typedecl_complex_type did nothing.
		if not getattr(t.type, "cpp_type", None): return
	else:
		anno_type_simple_type(t.type)

	if many:
		t.cpp_type = "std::vector<%s>" % t.type.cpp_type
		t.cpp_name = "%s_list" % t.name
	elif isinstance(t.type, XsdComplexType):
		t.cpp_type = "std::unique_ptr<%s>" % t.type.cpp_type
		t.cpp_name = "%s" % _(t.name)
	else:
		t.cpp_type = "%s" % t.type.cpp_type
		t.cpp_name = "%s" % _(t.name)

def anno_type_group(t: XsdGroup, many=False):
	if t.occurs[1] is None or t.occurs[1] > 1: many = True
	for e in t._group:
		if isinstance(e, XsdGroup):
			anno_type_group(e, many)
		elif isinstance(e, XsdElement):
			anno_type_element(e, many)
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

def anno_type_complex_type(t: XsdComplexType):
	if getattr(t, "cpp_type", None) is not None: return
	t.cpp_type = to_cpp_type(t.name)

	for attr in t.attributes.values():
		assert attr.use != "prohibited"
		anno_type_simple_type(attr.type)

	t.group_dfas = []
	if isinstance(t.content_type, XsdGroup):
		if len(t.content_type._group) > 0:
			assert t.content_type.model != "any", "<xs:any> is not supported."
			anno_type_group(t.content_type)
			if t.content_type.model == "all":
				t.group_dfas = [dfa_from_group(x) for x in t.content_type._group]
			else:
				t.group_dfas = [dfa_from_group(t.content_type)]
	else:
		anno_type_simple_type(t.content_type)

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

# Generates a string->enum function.
def lexer_from_enum(t: XsdAtomicRestriction) -> str:
	assert t.cpp_type.startswith("enum")
	out = ""
	out += "inline %s lex_%s(const char *in){\n" % (t.cpp_type, t.cpp_type[5:])
	for i, a in enumerate(t.validators[0].enumeration):
		out += "\t%s(strcmp(in, \"%s\") == 0){\n" % ("if" if i == 0 else "else if", a)
		out += "\t\treturn %s::%s;\n" % (t.cpp_type, to_enum_token(a))
		out += "\t}\n"
	out += "\telse throw std::runtime_error(\"Found unrecognized enum value \" + std::string(in) + \"of %s.\");\n" % t.cpp_type
	out += "}\n"
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

enum_lexers = ""
for e in enums:
	enum_lexers += lexer_from_enum(e) + "\n"

# Put the types found so far in unions in an enum. This is our tag enum.
type_tag_definition = "enum class type_tag {%s};\n" % ", ".join(set([to_enum_token(t.cpp_type) for t in simple_types]))

fn_declarations = ""
for t in types:
	fn_declarations += "void read_%s(pugi::xml_node &root, %s &out);\n" % (t.cpp_type, t.cpp_type)

print("#include <bitset>")
print("#include <cstring>")
print("#include <memory>")
print("#include <string>")
print("#include <vector>")
print("")
print("#include \"pugixml.hpp\"")
print("")
if struct_declarations: print(struct_declarations)
if enum_definitions: print(enum_definitions)
if enum_lexers: print(enum_lexers)
if simple_types: print(type_tag_definition)
if union_definitions: print(union_definitions)
if struct_definitions: print(struct_definitions)
if element_definitions: print(element_definitions)
if fn_declarations: print(fn_declarations , end="")

#

def gen_init_fn() -> str:
	out = """
void get_root_elements(const char *filename){
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

def _gen_load_simple(t: XsdSimpleType, container: str, input: str="node.child_value()") -> str:
	out = ""
	if t.cpp_type.startswith("std::vector"):
		out += "std::string raw, word;\n"
		out += "raw = %s;\n" % input
		out += "while(raw >> word){\n" % input
		out += "\t%s.emplace_back(%s);\n" % (container, atomic_builtin_load_formats[t.base_type.name] % "word")
		out += "}\n"
	elif t.cpp_type.startswith("enum"):
		out += "%s = lex_%s(%s);\n" % (container, t.cpp_type[5:], input) # "enum_A"[5:] == A
	else:
		out += "%s = %s;\n" % (container, atomic_builtin_load_formats[t.name] % input)
	return out

def _gen_load_element_complex(t: XsdElement, parent: str="") -> str:
	out = ""
	container = "%s%s" % ("%s." % parent if parent else "", t.cpp_name)
	if t.cpp_type.startswith("std::vector"):
		out += "%s.emplace_back(read_%s(node));\n" % (container, t.type.cpp_type)
	elif t.cpp_type.startswith("std::unique_ptr"):
		out += "%s = std::make_unique<%s>(read_%s(node));\n" % (container, t.type.cpp_type, t.type.cpp_type)
	else:
		raise ValueError("It should be impossible for an XsdElement with a complex type to have C++ type %s." % t.cpp_type)
	return out

def _gen_load_element(t: XsdElement, parent: str="") -> str:
	if isinstance(t.type, XsdComplexType):
		return _gen_load_element_complex(t, parent)
	elif isinstance(t.type, XsdAtomicBuiltin):
		container = "%s%s" % ("%s." % parent if parent else "", t.cpp_name)
		if t.cpp_type.startswith("std::vector"):
			return "%s.emplace_back(%s);\n" % (container, atomic_builtin_load_formats[t.type.name] % "node.child_value()")
		return "%s = %s;\n" % (container, atomic_builtin_load_formats[t.type.name] % "node.child_value()")
	else:
		raise NotImplementedError("%s?" % t.type)

#

# Hacky and brittle. The many-dfa implementation will be scrapped once again...
def _gen_load_group(t: XsdGroup) -> str:
	out = ""
	out += "int next, %s;\n" % ", ".join(["state%d = %s" % (i, dfa.start) for i, dfa in enumerate(t.group_dfas)])
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	for i, dfa in enumerate(t.group_dfas):
		out += "\tgtok_%s in = glex_%s(node.name());\n" % (t.cpp_type, t.cpp_type)
		out += "\tnext = gstates%d_%s[state%d][(int)in];\n" % (i, t.cpp_type, i)
		out += "\tif(next == -1) dfa_error(gtok_lookup_%s[(int)in], gstates%d_%s[state%d], gtok_lookup_%s, %d);\n"  % (t.cpp_type, i, t.cpp_type, i, t.cpp_type, len(dfa.alphabet))
		out += "\tstate%d = next;\n" % i

	elements = list(chain(*[dfa.elements.items() for dfa in t.group_dfas]))
	out += "\tswitch(in){\n";
	for inp, el in elements:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp_type, to_enum_token(inp))
		out += indent(_gen_load_element(el, "out"), 2) + "\n"
		out += "\t\tbreak;\n"
	out += "\t}\n";

	x = [" && ".join(["state%d != %s" % (i, x) for x in dfa.accepts]) for i, dfa in enumerate(t.group_dfas)]
	reject_cond = " || ".join(["(%s)" % cond for cond in x])
	out += "}\n"
	out += "if(%s) dfa_error(\"end of input\", gstates0_%s[state0], gtok_lookup_%s, %d);\n" % (reject_cond, t.cpp_type, t.cpp_type, len(dfa.alphabet))
	return out

def _gen_load_attrs(t: XsdGroup) -> str:
	out = ""
	N = len(t.attributes.values())
	out += "std::bitset<%d> astate = 0;\n" % N
	out += "for(pugi::xml_attribute attr = root.first_attribute(); attr; attr = attr.next_attribute()){\n"
	out += "\tatok_%s in = alex_%s(attr.name());\n" % (t.cpp_type, t.cpp_type)
	out += "\tif(astate[(int)in] == 0) astate[(int)in] = 1;\n"
	out += "\telse throw std::runtime_error(\"Duplicate attribute \" + std::string(attr.name()) + \" in <%s>.\");\n" % t.name

	out += "\tswitch(in){\n";
	for attr in t.attributes.values():
		out += "\tcase atok_%s::%s:\n" % (t.cpp_type, to_enum_token(attr.name))
		out += indent(_gen_load_simple(attr.type, "out.%s" % attr.name, "attr.value()"), 2) + "\n"
		out += "\t\tbreak;\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.use == "optional" else "0" for x in t.attributes.values()][::-1])
	out += "std::bitset<%d> test_state = astate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_state.all()) attr_error(test_state, atok_lookup_%s);\n" % t.cpp_type
	return out

# Generate enums for input tokens.
# Also generate a reverse lookup table for printing errors.
# TODO: Maybe flatten the alphabet while making the dfa.
def _gen_tokens(t: XsdComplexType) -> str:
	out = ""
	if t.group_dfas:
		enum_tokens = set(chain(*[[to_enum_token(w) for w in x.alphabet] for x in t.group_dfas]))
		lookup_tokens = set(chain(*[["\"%s\"" % w for w in x.alphabet] for x in t.group_dfas]))
		out += "enum class gtok_%s {%s};\n" % (t.cpp_type, ", ".join(enum_tokens))
		out += "const char *gtok_lookup_%s[] = {%s};\n" % (t.cpp_type, ", ".join(lookup_tokens))
	if t.attributes:
		enum_tokens = [to_enum_token(x.name) for x in t.attributes.values()]
		lookup_tokens = ["\"%s\"" % x.name for x in t.attributes.values()]
		out += "enum class atok_%s {%s};\n" % (t.cpp_type, ", ".join(enum_tokens))
		out += "const char *atok_lookup_%s[] = {%s};\n" % (t.cpp_type, ", ".join(lookup_tokens))
	return out

# Generate state transition tables, indexed by token enums.
# May cause a bug: sets aren't guaranteed to be ordered.
def _gen_state_tables(t: XsdComplexType) -> str:
	out = ""
	for i, dfa in enumerate(t.group_dfas):
		out += "int gstates%d_%s[%d][%d] = {\n" % (i, t.cpp_type, len(dfa.states), len(dfa.alphabet))
		for i in range(0, max(dfa.states)+1):
			state = dfa.transitions[i]
			row = [str(state[x]) if state.get(x) is not None else "-1" for x in dfa.alphabet]
			out += "\t{%s},\n" % ", ".join(row)
		out += "};\n"
	return out

# Generate lexing functions for children and attrs.
def _gen_lexer_fns(t: XsdComplexType) -> str:
	out = ""
	if t.group_dfas:
		alphabet = set(chain(*[x.alphabet for x in t.group_dfas]))
		out += "inline gtok_%s glex_%s(const char *in){\n" % (t.cpp_type, t.cpp_type)
		for i, a in enumerate(alphabet):
			out += "\t%s(strcmp(in, \"%s\") == 0){\n" % ("if" if i == 0 else "else if", a)
			out += "\t\treturn gtok_%s::%s;\n" % (t.cpp_type, to_enum_token(a))
			out += "\t}\n"
		out += "\telse throw std::runtime_error(\"Found unrecognized child \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	if t.attributes:
		out += "inline atok_%s alex_%s(const char *in){\n" % (t.cpp_type, t.cpp_type)
		for i, a in enumerate(t.attributes.values()):
			out += "\t%s(strcmp(in, \"%s\") == 0){\n" % ("if" if i == 0 else "else if", a.name)
			out += "\t\treturn atok_%s::%s;\n" % (t.cpp_type, to_enum_token(a.name))
			out += "\t}\n"
		out += "\telse throw std::runtime_error(\"Found unrecognized attribute \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	return out

def gen_complex_type_fn(t: XsdComplexType) -> str:
	out = ""
	out += _gen_tokens(t)
	out += _gen_lexer_fns(t)
	out += _gen_state_tables(t)
	out += "%s read_%s(const pugi::xml_node &root){\n" % (t.cpp_type, t.cpp_type)
	out += "\t%s out;\n" % t.cpp_type

	if t.group_dfas:
		out += indent(_gen_load_group(t)) + "\n"
	else:
		out += "\tif(root.first_child().type() == pugi::node_element) throw std::runtime_error(\"Unexpected child element in <%s>.\");\n" % t.name
		if t.has_simple_content():
			out += indent(_gen_load_simple(t.content_type, "out.value", "root.child_value()")) + "\n"

	if t.attributes:
		out += indent(_gen_load_attrs(t)) + "\n"
	else:
		out += "\tif(root.first_attribute()) std::runtime_error(\"Unexpected attribute in <%s>.\");\n" % t.name

	out += "\treturn out;\n"
	out += "}\n"
	return out

#

# Generate enums, tokenizers, state tables and function definitions.

print("""
/* runtime error for state machines */
void dfa_error(const char *wrong, int *states, const char **lookup, int len){
	std::vector<std::string> expected;
	for(int i=0; i<len; i++){
		if(states[i] != -1) expected.push_back(lookup[i]);
	}

	std::string expected_or = expected[0];
	for(int i=1; i<expected.size(); i++)
		expected_or += std::string(" or ") + expected[i];

	throw std::runtime_error("Expected " + expected_or + ", found " + std::string(wrong));
}
""")

print("""
/* runtime error for attributes */
template<std::size_t N>
void attr_error(std::bitset<N> astate, const char **lookup){
	std::vector<std::string> missing;
	for(int i=0; i<astate.size(); i++){
		if(astate[i] == 0) missing.push_back(lookup[i]);
	}

	std::string missing_and = missing[0];
	for(int i=1; i<missing.size(); i++)
		missing_and += std::string(" and ") + missing[i];

	throw std::runtime_error("Didn't find required attributes " + missing_and + ".");
}
""")

for t in types:
	print(gen_complex_type_fn(t))

print(gen_init_fn())
