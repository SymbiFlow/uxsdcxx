from typing import List, Tuple, Dict, Set, Union, Optional

from uxsdcxx import cpp_templates, utils, __version__
from uxsdcxx.third_party import triehash
from uxsdcxx.schema import (
	UxsdSchema,
	UxsdUnion,
	UxsdComplex,
	UxsdDfa,
	UxsdAll,
	UxsdLeaf,
	UxsdElement,
	UxsdType,
	UxsdEnum,
	UxsdSimple,
	UxsdAtomic,
	UxsdAttribute,
)

def typedefn_from_union(t: UxsdUnion) -> str:
	"""Generate a C++ definition of a type-tagged union.

	Example:
	/**
	 * Generated from:
	 * <xs:union... />
	*/
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
	out = ""
	out += "/** Generated from:\n"
	out += utils.to_comment_body(t.source)
	out += "\n*/\n"
	out += "struct %s {\n" % t.cpp
	out += "\ttype_tag tag;\n"
	out += "\tunion {\n"
	for e in t.member_types:
		field_name = utils.to_union_field_name(e.cpp)
		out += "\t\t%s %s;\n" % (e.cpp, field_name)
	out += "\t};\n"
	out += "};\n"
	return out

def typedefn_from_complex_type(t: UxsdComplex) -> str:
	"""Generate a C++ struct definition corresponding to a UxsdComplex.

	Example:
	/**
	 * Generated from:
	 * <xs:complexType... />
	*/
	struct t_foo {
		int bar;
		bool has_my_baz;
		t_baz my_baz;
		collapsed_vec<t_quux, quux_pool> my_quuxes;
	}
	"""
	fields = []
	for attr in t.attrs:
		fields.append("%s %s;" % (attr.type.cpp, utils.checked(attr.name)))
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			if el.many:
				fields.append("collapsed_vec<%s, %s_pool> %s;" % (el.type.cpp, el.type.name, utils.pluralize(el.name)))
			elif el.optional:
				fields.append("bool has_%s;" % el.name)
				fields.append("%s %s;" % (el.type.cpp, utils.checked(el.name)))
			else:
				fields.append("%s %s;" % (el.type.cpp, utils.checked(el.name)))
	elif isinstance(t.content, UxsdLeaf):
		fields.append("%s value;" % t.content.type.cpp)

	out = ""
	out += "/** Generated from:\n"
	out += utils.to_comment_body(t.source)
	out += "\n*/\n"
	out += "struct %s {\n" % t.cpp
	out += utils.indent("\n".join(fields))
	out += "\n};"
	return out

def typedefn_from_root_element(el: UxsdElement) -> str:
	"""Generate a C++ class declaration of a root element,
	which inherits its content type and adds load and write functions.
	"""
	out = ""
	out += "/** Generated from:\n"
	out += utils.to_comment_body(el.source)
	out += "\n*/\n"
	out += "class %s : public %s {\n" % (el.name, el.type.cpp)
	out += "public:\n"
	out += "\tpugi::xml_parse_result load(std::istream &is);\n"
	out += "\tvoid write(std::ostream &os);\n"
	out += "};\n"
	return out

#

def gen_clear_pools(ts: List[UxsdType]) -> str:
	out = "void clear_pools(void){\n"
	for t in ts:
		out += "\t%s_pool.clear();\n" % t.name
	out += "}"
	return out

#

def tokens_from_enum(t: UxsdEnum) -> str:
	"""Generate C++ enum of token values from an UxsdEnum"""
	out = ""
	enum_tokens = ["UXSD_INVALID = 0"]
	enum_tokens += [utils.to_token(x) for x in t.enumeration]
	out += "enum class %s {%s};" % (t.cpp, ", ".join(enum_tokens))
	return out

def lookup_from_enum(t: UxsdEnum) -> str:
	"""Generate C++ lookup table of tokens to strings from an UxsdEnum"""
	out = ""
	lookup_tokens = ["\"UXSD_INVALID\""]
	lookup_tokens += ["\"%s\"" % x for x in t.enumeration]
	out += "const char *lookup_%s[] = {%s};" % (t.name, ", ".join(lookup_tokens))
	return out

def lexer_from_enum(t: UxsdEnum) -> str:
	"""Generate a C++ function to convert const char *s to enum values generated
	from an UxsdEnum.

	It's in the form of enum_foo lex_enum_foo(const char *in, bool throw_on_invalid)
	and currently uses a trie to parse the string.
	throw_on_invalid is a hacky parameter to determine if we should throw on
	an invalid value. It's currently necessary to read unions - we don't need to
	throw on an invalid value if we are trying to read into an union but we need
	to throw otherwise.
	"""
	out = ""
	out += "inline %s lex_%s(const char *in, bool throw_on_invalid){\n" % (t.cpp, t.cpp)
	triehash_alph = [(x, "%s::%s" % (t.cpp, utils.to_token(x))) for x in t.enumeration]
	out += utils.indent(triehash.gen_lexer_body(triehash_alph))
	out += "\tif(throw_on_invalid)\n"
	out += "\t\tthrow std::runtime_error(\"Found unrecognized enum value \" + std::string(in) + \" of %s.\");\n" % t.cpp
	out += "\treturn %s::UXSD_INVALID;\n" % t.cpp
	out += "}\n"
	return out

def tokens_from_complex_type(t: UxsdComplex) -> str:
	"""Generate one or two C++ enums of token values from an UxsdComplex.
	One enum is generated from valid attribute names and the other from child element names.
	"""
	out = ""
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		enum_tokens = [utils.to_token(e.name) for e in t.content.children]
		lookup_tokens = ["\"%s\"" % e.name for e in t.content.children]
		out += "enum class gtok_%s {%s};\n" % (t.cpp, ", ".join(enum_tokens))
		out += "const char *gtok_lookup_%s[] = {%s};" % (t.cpp, ", ".join(lookup_tokens))
	if t.attrs:
		enum_tokens = [utils.to_token(x.name) for x in t.attrs]
		lookup_tokens = ["\"%s\"" % x.name for x in t.attrs]
		out += "enum class atok_%s {%s};\n" % (t.cpp, ", ".join(enum_tokens))
		out += "const char *atok_lookup_%s[] = {%s};\n" % (t.cpp, ", ".join(lookup_tokens))
	return out

def lexer_from_complex_type(t: UxsdComplex) -> str:
	"""Generate one or two C++ functions to convert const char *s to enum values
	generated from an UxsdComplex.

	It's in the form of (a|g)tok_foo lex_(attr|node)_foo(const char *in) and currently uses
	a trie to lex the string. a or g indicates if the token is an attribute token or a group
	(child element) token.
	"""
	out = ""
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		out += "inline gtok_%s lex_node_%s(const char *in){\n" % (t.cpp, t.cpp)
		triehash_alph = [(e.name, "gtok_%s::%s" % (t.cpp, utils.to_token(e.name))) for e in t.content.children]
		out += utils.indent(triehash.gen_lexer_body(triehash_alph))
		out += "\tthrow std::runtime_error(\"Found unrecognized child \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	if t.attrs:
		out += "inline atok_%s lex_attr_%s(const char *in){\n" % (t.cpp, t.cpp)
		triehash_alph = [(x.name, "atok_%s::%s" % (t.cpp, utils.to_token(x.name))) for x in t.attrs]
		out += utils.indent(triehash.gen_lexer_body(triehash_alph))
		out += "\tthrow std::runtime_error(\"Found unrecognized attribute \" + std::string(in) + \" of <%s>.\");\n" % t.name
		out += "}\n"
	return out

#

def _gen_dfa_table(t: UxsdComplex) -> str:
	"""Generate a 2D C++ array representing DFA table from an UxsdComplex's DFA.

	The array is indexed by the state and input token value, such that table[state][input]
	gives the next state.
	"""
	assert isinstance(t.content, UxsdDfa)
	dfa = t.content.dfa
	out = ""
	out += "static const int NUM_%s_STATES = %d;\n" % (t.cpp.upper(), len(dfa.states))
	out += "static const int NUM_%s_INPUTS = %d;\n" % (t.cpp.upper(), len(dfa.alphabet))
	out += "int gstate_%s[NUM_%s_STATES][NUM_%s_INPUTS] = {\n" % (t.cpp, t.cpp.upper(), t.cpp.upper())
	for i in range(0, max(dfa.states)+1):
		state = dfa.transitions[i]
		row = [str(state[x]) if state.get(x) is not None else "-1" for x in dfa.alphabet]
		out += "\t{%s},\n" % ", ".join(row)
	out += "};\n"
	return out

# TODO: Find a cleaner way to load unions.
def _gen_load_union(t: UxsdUnion, container: str, input: str) -> str:
	out = ""
	for m in t.member_types:
		new_container = "%s.%s" % (container, utils.to_union_field_name(m.cpp))
		out += "%s.tag = type_tag::%s;\n" % (container, utils.to_token(m.cpp))
		if isinstance(m, UxsdAtomic):
			out += "%s = %s;\n" % (new_container, (m.cpp_load_format % input))
			out += "if(errno == 0)\n"
			out += "\tbreak;\n"
		elif isinstance(m, UxsdEnum):
			out += "%s = lex_%s(%s, false);\n" % (new_container, m.cpp, input)
			out += "if(%s != %s::UXSD_INVALID)\n" % (new_container, m.cpp)
			out += "break;\n"
		else:
			raise NotImplementedError("I don't know how to load %s into a union." % m)
	out += "throw std::runtime_error(\"Couldn't load a suitable value into union %s.\");\n" % t.name
	return out

# See https://stackoverflow.com/questions/26080829/detecting-strtol-failure
# Since detecting additional characters require some other hoops which would
# hurt performance, we only check errno.
def _gen_load_simple(t: UxsdSimple, container: str, input: str) -> str:
	out = ""
	if isinstance(t, UxsdAtomic):
		out += "%s = %s;\n" % (container, (t.cpp_load_format % input))
		out += "if(errno != 0)\n"
		out += "\tthrow std::runtime_error(\"Invalid value `\" + std::string(%s) + \"` to load a %s into %s.\");\n" % (input, t.cpp, container)
	elif isinstance(t, UxsdEnum):
		out += "%s = lex_%s(%s, true);\n" % (container, t.cpp, input)
	elif isinstance(t, UxsdUnion):
		out += _gen_load_union(t, container, input)
	else:
		raise TypeError("Unknown simple type %s." % t)
	return out

def _gen_load_element_complex(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdComplex)
	out = ""
	if t.many:
		out += "%s->%s.push_back(%s());\n" % (parent, utils.pluralize(t.name), t.type.cpp)
		out += "load_%s(node, &%s->%s.back());\n" % (t.type.name, parent, utils.pluralize(t.name))
	else:
		out += "load_%s(node, &%s->%s);\n" % (t.type.name, parent, t.name)
		if t.optional:
			out += "%s->has_%s = 1;\n" % (parent, t.name)
	return out

def _gen_load_element_simple(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdSimple)
	out = ""
	if t.many:
		container = "%s->%s" % (parent, utils.pluralize(t.name))
		out += "%s.push_back(0);\n" % container
		out += _gen_load_simple(t.type, "%s.back()" % container, "node.child_value()")
	else:
		container = "%s->%s" % (parent, utils.checked(t.name))
		out += _gen_load_simple(t.type, container, "node.child_value()")
		if t.optional:
			out += "%s->has_%s = 1;\n" % (parent, t.name)
	return out

def _gen_load_element(t: UxsdElement, parent: str) -> str:
	if isinstance(t.type, UxsdComplex):
		return _gen_load_element_complex(t, parent)
	else:
		return _gen_load_element_simple(t, parent)

def _gen_load_dfa(t: UxsdComplex) -> str:
	"""Partial function to generate the child element validation&loading portion
	of a C++ function load_foo, if the model group is an xs:sequence or xs:choice.

	xs:sequence/xs:choice groups can be compiled down to a finite automaton.
	This is done in dfa.py. C++ state table is generated in _gen_dfa_table and the
	stream of child elements are validated according to the table here.

	The C++ table has -1s in place of invalid state transitions. If we step into a -1,
	we call dfa_error. We check again at the end of input. If we aren't in an accepted
	state, we again call dfa_error.
	"""
	assert isinstance(t.content, UxsdDfa)
	dfa = t.content.dfa
	out = ""
	out += "int next, state=%d;\n" % dfa.start
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	out += "\tgtok_%s in = lex_node_%s(node.name());\n" % (t.cpp, t.cpp)

	out += "\tnext = gstate_%s[state][(int)in];\n" % t.cpp
	out += "\tif(next == -1)\n"
	out += "\t\tdfa_error(gtok_lookup_%s[(int)in], gstate_%s[state], gtok_lookup_%s, %d);\n"\
					% (t.cpp, t.cpp, t.cpp, len(dfa.alphabet))
	out += "\tstate = next;\n"

	out += "\tswitch(in){\n";
	for el in t.content.children:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp, utils.to_token(el.name))
		out += utils.indent(_gen_load_element(el, "out"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";

	reject_cond = " && ".join(["state != %d" % x for x in dfa.accepts])
	out += "}\n"
	out += "if(%s) dfa_error(\"end of input\", gstate_%s[state], gtok_lookup_%s, %d);\n"\
			% (reject_cond, t.cpp, t.cpp, len(dfa.alphabet))

	return out

def _gen_load_all(t: UxsdComplex) -> str:
	"""Partial function to generate the child element validation&loading portion
	of a C++ function load_foo, if the model group is an xs:all.

	xs:alls can be validated in a similar fashion to xs:attributes. We maintain a
	bitset of which elements are found. At the end, we OR our bitset with the value
	corresponding to the optional elements and check if all bits in it are set. If not,
	we call attr_error with the token lookup table and the OR'd bitset.
	"""
	assert isinstance(t.content, UxsdAll)
	N = len(t.content.children)
	out = ""

	out += "std::bitset<%d> gstate = 0;\n" % N
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	out += "\tgtok_%s in = lex_node_%s(node.name());\n" % (t.cpp, t.cpp)

	out += "\tif(gstate[(int)in] == 0) gstate[(int)in] = 1;\n"
	out += "\telse throw std::runtime_error(\"Duplicate element \" + std::string(node.name()) + \" in <%s>.\");\n" % t.name

	out += "\tswitch(in){\n";
	for el in t.content.children:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp, utils.to_token(el.name))
		out += utils.indent(_gen_load_element(el, "out"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.optional else "0" for x in t.content.children][::-1])
	out += "std::bitset<%d> test_gstate = gstate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_gstate.all()) all_error(test_gstate, gtok_lookup_%s);\n" % t.cpp

	return out

def _gen_load_attrs(t: UxsdComplex) -> str:
	"""Partial function to generate the attribute loading portion of a C++
	function load_foo. See _gen_load_all to see how attributes are validated.
	"""
	assert len(t.attrs) > 0
	N = len(t.attrs)
	out = ""
	out += "std::bitset<%d> astate = 0;\n" % N
	out += "for(pugi::xml_attribute attr = root.first_attribute(); attr; attr = attr.next_attribute()){\n"
	out += "\tatok_%s in = lex_attr_%s(attr.name());\n" % (t.cpp, t.cpp)
	out += "\tif(astate[(int)in] == 0) astate[(int)in] = 1;\n"
	out += "\telse throw std::runtime_error(\"Duplicate attribute \" + std::string(attr.name()) + \" in <%s>.\");\n" % t.name

	out += "\tswitch(in){\n";
	for attr in t.attrs:
		out += "\tcase atok_%s::%s:\n" % (t.cpp, utils.to_token(attr.name))
		out += utils.indent(_gen_load_simple(attr.type, "out->%s" % utils.checked(attr.name), "attr.value()"), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.optional else "0" for x in t.attrs][::-1])
	out += "std::bitset<%d> test_astate = astate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_astate.all()) attr_error(test_astate, atok_lookup_%s);\n" % t.cpp
	return out

def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""Generate a full C++ function load_foo(&root, *out)
	which can load an XSD complex type from DOM &root into C++ type *out.
	"""
	out = ""

	out += "void load_%s(const pugi::xml_node &root, %s *out){\n" % (t.name, t.cpp)
	if isinstance(t.content, UxsdDfa):
		out = _gen_dfa_table(t) + out
		out += utils.indent(_gen_load_dfa(t))
	elif isinstance(t.content, UxsdAll):
		out += utils.indent(_gen_load_all(t))
	elif isinstance(t.content, UxsdLeaf):
		out += utils.indent(_gen_load_simple(t.content.type, "out->value", "root.child_value()"))
	else:
		out += "\tif(root.first_child().type() == pugi::node_element)\n"
		out += "\t\tthrow std::runtime_error(\"Unexpected child element in <%s>.\");\n" % t.name

	out += "\n"
	if t.attrs:
		out += utils.indent(_gen_load_attrs(t))
	else:
		out += "\tif(root.first_attribute())\n"
		out += "\t\tthrow std::runtime_error(\"Unexpected attribute in <%s>.\");\n" % t.name

	out += "}\n"
	return out

#

def load_fn_from_element(el: UxsdElement) -> str:
	out = ""
	out += "pugi::xml_parse_result %s::load(std::istream &is){\n" % utils.checked(el.name)
	out += "\tpugi::xml_document doc;\n"
	out += "\tpugi::xml_parse_result result = doc.load(is);\n"
	out += "\tif(!result) return result;\n"
	out += "\tfor(pugi::xml_node node= doc.first_child(); node; node = node.next_sibling()){\n"

	out += "\t\tif(std::strcmp(node.name(), \"%s\") == 0){\n" % el.name
	out += "\t\t\t/* If errno is set up to this point, it messes with strtol errno checking. */\n"
	out += "\t\t\terrno = 0;\n"
	out += "\t\t\tload_%s(node, this);\n" % el.type.name

	out += "\t\t}\n"
	out += "\t\telse throw std::runtime_error(\"Invalid root-level element \" + std::string(node.name()));\n"
	out += "\t}\n"
	out += "\treturn result;\n"
	out += "}\n"
	return out

#

def _gen_write_simple(t: UxsdSimple, container: str, attr_name: str="") -> str:
	"""Partial function to generate code which writes out a simple type.

	The attr_name parameter is passed by _gen_write_attr so that we can
	generate squashed code like `os << "index=\"" << y_list.index << "\"";`.
	"""
	out = ""
	if isinstance(t, UxsdAtomic):
		if attr_name:
			out += "os << \" %s=\\\"\" << %s << \"\\\"\";\n" % (attr_name, container)
		else:
			out += "os << %s;\n" % container
	elif isinstance(t, UxsdEnum):
		if attr_name:
			out += "os << \" %s=\\\"\" << lookup_%s[(int)%s] << \"\\\"\";\n" % (attr_name, t.name, container)
		else:
			out += "os << lookup_%s[(int)%s];\n" % (t.name, container)
	elif isinstance(t, UxsdUnion):
		for m in t.member_types:
			out += "if(%s.tag == type_tag::%s)" % (container, utils.to_token(m.cpp))
			out += utils.indent(_gen_write_simple(t, container + "." + utils.to_union_field_name(m.cpp), attr_name))
	else:
		raise NotImplementedError("I don't know how to write out %s." % t)
	return out

def _gen_write_attr(a: UxsdAttribute, container: str) -> str:
	"""Partial function to generate code which writes out a single XML attribute."""
	out = ""
	new_container = "%s.%s" % (container, a.name)
	if not a.optional or a.default_value:
		out += _gen_write_simple(a.type, new_container, a.name)
	else:
		out += "if((bool)%s)\n" % new_container
		out += utils.indent(_gen_write_simple(a.type, new_container, a.name))
	return out

def _gen_write_complex(t: UxsdComplex, name: str, container: str) -> str:
	"""Partial function to generate code which writes out a simple type."""
	out = ""
	if t.attrs:
		out += "os << \"<%s\";\n" % name
		for a in t.attrs:
			out += _gen_write_attr(a, container)
	else:
		out += "os << \"<%s\";\n" % name

	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		out += "os << \">\";\n"
		for e in t.content.children:
			if e.many:
				out += "for(auto &%s: %s.%s){\n" % (utils.checked(e.name), container, utils.pluralize(e.name))
				out += utils.indent(_gen_write_element(e, utils.checked(e.name)))
				out += "}\n"
			else:
				new_container = "%s.%s" % (container, utils.checked(e.name))
				if e.optional:
					out += "if(%s.has_%s){\n" % (container, e.name)
					out += utils.indent(_gen_write_element(e, new_container))
					out += "}\n"
				else:
					out += _gen_write_element(e, new_container)
		out += "os << \"</%s>\";\n" % name
	elif isinstance(t.content, UxsdLeaf):
		out += "os << \">\";\n"
		out += _gen_write_simple(t.content.type, container+".value")
		out += "os << \"</%s>\";\n" % name
	else:
		out += "os << \"/>\";\n"
	return out


def _gen_write_element(el: UxsdElement, container: str) -> str:
	"""Partial function to generate C++ code for writing out a struct generated
	from an UxsdElement.

	Currently, all values with non-zero default values are emitted.
	Otherwise, we would have to check against the nonzero value, and the
	check would create a case split for all simple types again.(how to compare
	unions? strings? doubles?)
	"""
	out = ""
	if isinstance(el.type, UxsdSimple):
		out += "os << \"<%s>\";\n" % el.name
		out += _gen_write_simple(el.type, container)
		out += "os << \"</%s>\";\n" % el.name
	elif isinstance(el.type, UxsdComplex):
		out += _gen_write_complex(el.type, el.name, container)
	else:
		raise TypeError("Unknown type %s." % el.type)
	return out

def write_fn_from_element(t: UxsdElement) -> str:
	out = ""
	out += "void %s::write(std::ostream &os){\n" % utils.checked(t.name)
	out += utils.indent(_gen_write_element(t, "(*this)"))
	out += "}\n"
	return out

#

def render_header_file(schema: UxsdSchema, cmdline: str, input_file: str) -> str:
	"""Render a C++ header file to a string."""
	out = ""
	x = {"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)}
	out += cpp_templates.header_comment.substitute(x)
	out += cpp_templates.includes
	out += cpp_templates.collapsed_vec_defn
	out += cpp_templates.char_pool_defn
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {"

	out += "\n\n/* Forward decl of generated data types. Needed for the pools.\n"
	out += " * The types are sorted according to tree height, so that the \"root type\"\n"
	out += " * appears last and we don't get any \"incomplete type\" errors. */\n"
	struct_decls = ["struct %s;" % t.cpp for t in schema.unions] + ["struct %s;" % t.cpp for t in schema.complex_types]
	out += "\n".join(struct_decls)

	out += "\n\n/* Global shared pools for storing multiply-occurring elements. */\n"
	extern_pool_decls = []
	for t in schema.pool_types:
		extern_pool_decls.append("extern std::vector <%s> %s_pool;" % (t.cpp, t.name))
	out += "\n".join(extern_pool_decls)
	out += "\n"
	if schema.has_string:
		out += "\nextern char_pool_impl char_pool;\n"
	out += "\n/* Helper function for freeing the pools. */\n"
	out += "void clear_pools(void);"
	if schema.has_string:
		out += "\n/* One may want to use the allocated strings after loading, so this\n"
		out += " * function is provided separately. */\n"
		out += "void clear_strings(void);"

	out += "\n\n/* Enum tokens generated from XSD enumerations. */\n"
	enum_tokens = [tokens_from_enum(t) for t in schema.enums]
	out += "\n".join(enum_tokens)

	type_tag_tokens = [utils.to_token(x.cpp) for x in schema.simple_types_in_unions]
	if type_tag_tokens:
		out += "\n\n/* Type tag enum for tagged unions. */\n"
		out += "enum class type_tag {%s};\n" % ", ".join(type_tag_tokens)

	union_defns = [typedefn_from_union(t) for t in schema.unions]
	if union_defns:
		out += "\n\n/* Structs generated from  XSD unions. */\n"
		out += "\n\n".join(union_defns)

	struct_defns = [typedefn_from_complex_type(t) for t in schema.complex_types]
	out += "\n\n/* Structs generated from complex types. */\n\n"
	out += "\n\n".join(struct_defns)
	root_element_decls = [typedefn_from_root_element(el) for el in schema.root_elements]
	out += "\n\n/* Classes generated from root elements. */\n"
	out += "\n\n".join(root_element_decls)
	out += "\n} /* namespace uxsd */\n"
	return out

def render_impl_file(schema: UxsdSchema, cmdline: str, input_file: str, header_file_name: str) -> str:
	"""Render a C++ implementation file to a string."""
	out = ""
	x = {"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)}
	out += cpp_templates.header_comment.substitute(x)
	out += "#include \"%s\"" % header_file_name
	out += "\n\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {\n\n"
	out += triehash.gen_prelude()

	out += "\n/* Declarations for internal load functions for the root elements. */\n"
	load_fn_decls = []
	for t in schema.complex_types:
		load_fn_decls.append("void load_%s(const pugi::xml_node &root, %s *out);" % (t.name, t.cpp))
	out += "\n".join(load_fn_decls)

	pool_decls = ["std::vector<%s> %s_pool;" % (t.cpp, t.name) for t in schema.pool_types]
	if pool_decls:
		out += "\n"
		out += "\n".join(pool_decls)
	if schema.has_string:
		out += "\nchar_pool_impl char_pool;\n"
	if pool_decls:
		out += "\n"
		out += gen_clear_pools(schema.pool_types)
	if schema.has_string:
		out += "\nvoid clear_strings(void){\n"
		out += "\tchar_pool.clear();\n"
		out += "}"

	if schema.enums:
		enum_lookups = [lookup_from_enum(t) for t in schema.enums]
		enum_lexers = [lexer_from_enum(t) for t in schema.enums]
		out += "\n\n/* Lookup tables for enums. */\n"
		out += "\n".join(enum_lookups)
		out += "\n\n/* Lexers(string->token functions) for enums. */\n"
		out += "\n".join(enum_lexers)

	root_load_defns = [load_fn_from_element(el) for el in schema.root_elements]
	out += "\n\n/* Load functions for the root elements. */\n"
	out += "\n".join(root_load_defns)
	root_write_defns = [write_fn_from_element(el) for el in schema.root_elements]
	out += "\n\n/* Write functions for the root elements. */\n"
	out += "\n".join(root_write_defns)

	complex_type_tokens = [tokens_from_complex_type(t) for t in schema.complex_types]
	out += "\n\n/* Tokens for attribute and node names. */\n"
	out += "\n".join(complex_type_tokens)
	complex_type_lexers = [lexer_from_complex_type(t) for t in schema.complex_types]
	out += "\n\n/* Internal lexers. These convert the PugiXML node names to input tokens. */\n"
	out += "\n".join(complex_type_lexers)

	if schema.has_dfa:
		out += cpp_templates.dfa_error_decl
	if schema.has_all:
		out += cpp_templates.all_error_decl
	if schema.has_attr:
		out += cpp_templates.attr_error_decl
	complex_type_loaders = [load_fn_from_complex_type(t) for t in schema.complex_types]
	out += "\n\n/* Internal loading functions, which validate and load a PugiXML DOM tree into memory. */\n"
	out += "\n".join(complex_type_loaders)
	if schema.has_dfa:
		out += cpp_templates.dfa_error_defn
	if schema.has_all:
		out += cpp_templates.all_error_defn
	if schema.has_attr:
		out += cpp_templates.attr_error_defn
	out += "\n} /* namespace uxsd */\n"
	return out
