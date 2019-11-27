from typing import List, Tuple, Dict, Set, Union, Optional

from . import cpp_templates, utils
from .utils import checked
from .version import __version__
from .third_party import triehash
from .schema import (
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
	UxsdString,
	UxsdAtomic,
	UxsdAttribute,
)

def _gen_virtual_fns(t: UxsdComplex) -> str:
	"""Generate virtual functions to interface with an element with a complex type."""
	fields = []
	def _add_field(ret: str, verb: str, what: str, args: str):
		fields.append("virtual inline %s %s_%s_%s(%s) = 0;" % (ret, verb, t.name, what, args))

	def _add_set(e: Union[UxsdElement, UxsdAttribute]):
		_add_field("void", "set", e.name, "%s %s, void * data" % (e.type.cpp, checked(e.name)))
	def _add_add_simple(e: UxsdElement):
		_add_field("void", "add", e.name, "%s %s, void * data" % (e.type.cpp, checked(e.name)))
	def _add_add_complex(e: UxsdElement):
		_add_field("void *", "add", e.name, "void * data")
	def _add_init(e: UxsdElement):
		_add_field("void *", "init", e.name, "void * data")

	def _add_get_simple(e: Union[UxsdElement, UxsdAttribute]):
		_add_field(e.type.cpp, "get", e.name, "void * data")
	def _add_get_simple_many(e: UxsdElement):
		_add_field(e.type.cpp, "get", e.name, "int n, void * data")
	def _add_get_complex(e: UxsdElement):
		_add_field("void *", "get", e.name, "void * data")
	def _add_get_complex_many(e: UxsdElement):
		_add_field("void *", "get", e.name, "int n, void * data")
	def _add_num(e: UxsdElement):
		_add_field("size_t", "num", e.name, "void * data")
	def _add_has(e: UxsdElement):
		_add_field("bool", "has", e.name, "void * data")

	for attr in t.attrs:
		_add_set(attr)
		_add_get_simple(attr)
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for e in t.content.children:
			if isinstance(e.type, UxsdComplex):
				if e.many:
					_add_add_complex(e)
					_add_num(e)
					_add_get_complex_many(e)
				else:
					_add_init(e)
					_add_get_complex(e)
					if e.optional: _add_has(e)
			elif isinstance(e.type, UxsdSimple):
				if e.many:
					_add_add_simple(e)
					_add_num(e)
					_add_get_simple_many(e)
				else:
					_add_set(e)
					_add_get_simple(e)
			else:
				raise TypeError(e)
	elif isinstance(t.content, UxsdLeaf):
		_add_field("void", "set", "value", "%s value, void * data" % t.content.type.cpp)
		_add_field(t.content.type.cpp, "get", "value", "void * data")

	out = ""
	out += "/** Generated for complex type \"%s\":\n" % t.name
	out += utils.to_comment_body(t.source)
	out += "\n*/\n"
	out += "\n".join(fields)
	return out

def gen_base_class(schema: UxsdSchema) -> str:
	"""Generate a C++ base class of a root element."""
	out = ""
	root = schema.root_element
	out += "class %sBase {\n" % utils.to_pascalcase(root.name)
	out += "public:\n"
	virtual_fns = [_gen_virtual_fns(x) for x in schema.complex_types]
	out += utils.indent("\n\n".join(virtual_fns))
	out += "\n};\n"
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
		out += "static const char *gtok_lookup_%s[] = {%s};" % (t.cpp, ", ".join(lookup_tokens))
	if t.attrs:
		enum_tokens = [utils.to_token(x.name) for x in t.attrs]
		lookup_tokens = ["\"%s\"" % x.name for x in t.attrs]
		out += "enum class atok_%s {%s};\n" % (t.cpp, ", ".join(enum_tokens))
		out += "static const char *atok_lookup_%s[] = {%s};\n" % (t.cpp, ", ".join(lookup_tokens))
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
	out += "static int gstate_%s[NUM_%s_STATES][NUM_%s_INPUTS] = {\n" % (t.cpp, t.cpp.upper(), t.cpp.upper())
	for i in range(0, max(dfa.states)+1):
		state = dfa.transitions[i]
		row = [str(state[x]) if state.get(x) is not None else "-1" for x in dfa.alphabet]
		out += "\t{%s},\n" % ", ".join(row)
	out += "};\n"
	return out

def _gen_stub_suffix(t: Union[UxsdElement, UxsdAttribute], parent: str) -> str:
	return "%s_%s" % (parent, t.name)

def _gen_load_simple(t: UxsdSimple, input: str) -> str:
	if isinstance(t, UxsdString):
		return input
	elif isinstance(t, UxsdEnum):
		return "lex_%s(%s, true)" % (t.cpp, input)
	else:
		return "load_%s(%s)" % (utils.to_snakecase(t.cpp), input)

def _gen_load_element_complex(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdComplex)
	out = ""
	if t.many:
		out += "load_%s(node, out, out.add_%s(data));\n" % (t.type.name, _gen_stub_suffix(t, parent))
	else:
		out += "load_%s(node, out, out.init_%s(data));\n" % (t.type.name, _gen_stub_suffix(t, parent))
	return out

def _gen_load_element_simple(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdSimple)
	out = ""
	if t.many:
		out += "out.add_%s(%s, data);\n" % (_gen_stub_suffix(t, parent), _gen_load_simple(t.type, "node.child_value()"))
	else:
		out += "out.set_%s(%s, data);\n" % (_gen_stub_suffix(t, parent), _gen_load_simple(t.type, "node.child_value()"))
	return out

def _gen_load_element(t: UxsdElement, parent: str) -> str:
	if isinstance(t.type, UxsdComplex):
		return _gen_load_element_complex(t, parent)
	else:
		return _gen_load_element_simple(t, parent)

def _gen_load_attr(t: UxsdAttribute, parent: str) -> str:
	return "out.set_%s(%s, data);\n" % (_gen_stub_suffix(t, parent), _gen_load_simple(t.type, "attr.value()"))

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
	out += "\t\tdfa_error(gtok_lookup_%s[(int)in], gstate_%s[state], gtok_lookup_%s, %d);\n" % (t.cpp, t.cpp, t.cpp, len(dfa.alphabet))
	out += "\tstate = next;\n"

	out += "\tswitch(in){\n";
	for el in t.content.children:
		out += "\tcase gtok_%s::%s:\n" % (t.cpp, utils.to_token(el.name))
		out += utils.indent(_gen_load_element(el, t.name), 2)
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
		out += utils.indent(_gen_load_element(el, t.name), 2)
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
		out += utils.indent(_gen_load_attr(attr, t.name), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n";
	out += "}\n"

	mask = "".join(["1" if x.optional else "0" for x in t.attrs][::-1])
	out += "std::bitset<%d> test_astate = astate | std::bitset<%d>(0b%s);\n" % (N, N, mask)
	out += "if(!test_astate.all()) attr_error(test_astate, atok_lookup_%s);\n" % t.cpp
	return out

def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""Generate a full C++ function load_foo(&root, &out)
	which can load an XSD complex type from DOM &root into C++ object out.
	"""
	out = ""
	out += "template<class T>\n"
	out += "void load_%s(const pugi::xml_node &root, T &out, void *data){\n" % t.name
	if isinstance(t.content, UxsdDfa):
		out = _gen_dfa_table(t) + out
		out += utils.indent(_gen_load_dfa(t))
	elif isinstance(t.content, UxsdAll):
		out += utils.indent(_gen_load_all(t))
	elif isinstance(t.content, UxsdLeaf):
		out += "\tout.set_%s_value(%s, data);\n" % (t.name, _gen_load_simple(t.content.type, "root.child_value()"))

	if not isinstance(t.content, (UxsdDfa, UxsdAll)):
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

# See https://stackoverflow.com/questions/26080829/detecting-strtol-failure
# Since detecting additional characters require some other hoops which would
# hurt performance, we only check errno.
def load_fn_from_simple_type(t: UxsdSimple) -> str:
	"""Generate a full C++ function load_foo(str)
	which can load an XSD simple type from str and return it.
	"""
	out = ""
	out += "inline %s load_%s(const char *in){\n" % (t.cpp, utils.to_snakecase(t.cpp))
	out += "\t%s out;\n" % t.cpp
	if isinstance(t, UxsdAtomic):
		out += "\tout = %s;\n" % (t.cpp_load_format % "in")
		out += "\tif(errno != 0)\n"
		out += "\t\tthrow std::runtime_error(\"Invalid value `\" + std::string(in) + \"` when loading into a %s.\");" % t.cpp
	elif isinstance(t, UxsdEnum):
		out += "\tout = lex_%s(in, true);\n" % t.cpp
	else:
		raise TypeError("Unsupported simple type %s." % t)
	out += "\treturn out;\n"
	out += "}\n"
	return out

#

def load_fn_from_root_element(e: UxsdElement) -> str:
	out = ""
	out += "template <class T>\n"
	out += "pugi::xml_parse_result load_%s_xml(T &out, std::istream &is){\n" % e.name
	out += "static_assert(std::is_base_of<%sBase, T>::value, \"Base class not derived from %sBase\");\n" % (utils.to_pascalcase(e.name), utils.to_pascalcase(e.name))
	out += "\tpugi::xml_document doc;\n"
	out += "\tpugi::xml_parse_result result = doc.load(is);\n"
	out += "\tif(!result) return result;\n"
	out += "\tfor(pugi::xml_node node= doc.first_child(); node; node = node.next_sibling()){\n"

	out += "\t\tif(std::strcmp(node.name(), \"%s\") == 0){\n" % e.name
	out += "\t\t\t/* If errno is set up to this point, it messes with strtol errno checking. */\n"
	out += "\t\t\terrno = 0;\n"
	out += "\t\t\tload_%s(node, out, NULL);\n" % e.type.name

	out += "\t\t}\n"
	out += "\t\telse throw std::runtime_error(\"Invalid root-level element \" + std::string(node.name()));\n"
	out += "\t}\n"
	out += "\treturn result;\n"
	out += "}\n"
	return out

#

def _gen_check_simple(t: Union[UxsdElement, UxsdAttribute], parent: str, data: str = "data") -> str:
	if isinstance(t, UxsdElement) and t.many:
		return "in.get_%s(i, %s)" % (_gen_stub_suffix(t, parent), data)
	else:
		return "in.get_%s(%s)" % (_gen_stub_suffix(t, parent), data)

def _gen_write_simple(t: Union[UxsdElement, UxsdAttribute], parent: str, data: str = "data") -> str:
	if isinstance(t.type, UxsdAtomic):
		if isinstance(t, UxsdElement) and t.many:
			return "in.get_%s(i, %s)" % (_gen_stub_suffix(t, parent), data)
		else:
			return "in.get_%s(%s)" % (_gen_stub_suffix(t, parent), data)
	elif isinstance(t.type, UxsdEnum):
		if isinstance(t, UxsdElement) and t.many:
			return "lookup_%s[(int)in.get_%s(i, %s)]" % (t.type.name, _gen_stub_suffix(t, parent), data)
		else:
			return "lookup_%s[(int)in.get_%s(%s)]" % (t.type.name, _gen_stub_suffix(t, parent), data)
	else:
		raise NotImplementedError(t)

def _gen_write_attr(a: UxsdAttribute, parent: str, data: str = "data") -> str:
	"""Function to generate partial code which writes out a single XML attribute."""
	out = ""
	if not a.optional or a.default_value:
		out += "os << \" %s=\\\"\" << %s << \"\\\"\";\n" % (a.name, _gen_write_simple(a, parent, data))
	else:
		out += "if((bool)%s)\n" % _gen_check_simple(a, parent, data)
		out += "\tos << \" %s=\\\"\" << %s << \"\\\"\";\n" % (a.name, _gen_write_simple(a, parent, data))
	return out

def _gen_write_complex_element(e: UxsdElement, parent: str) -> str:
	"""Function to generate partial code which writes out an element with a complex type."""
	assert isinstance(e.type, UxsdComplex)
	out = ""

	def _gen_write_element_body() -> str:
		assert isinstance(e.type, UxsdComplex)
		ouv = ""
		if e.type.attrs:
			ouv += "os << \"<%s\";\n" % e.name
			for a in e.type.attrs:
				ouv += _gen_write_attr(a, e.name, "child_data")
			if e.type.content:
				ouv += "os << \">\";\n"
				ouv += "write_%s(in, os, child_data);\n" % e.type.name
				ouv += "os << \"</%s>\";\n" % e.name
			else:
				ouv += "os << \"/>\";\n"
		else:
			if e.type.content:
				ouv += "os << \"<%s>\";\n" % e.name
				ouv += "write_%s(in, os, child_data);\n" % e.type.name
				ouv += "os << \"</%s>\";\n" % e.name
			else:
				ouv += "os << \"<%s/>\";\n" % e.name
		return ouv

	if e.many:
		out += "for(size_t i=0, n=in.num_%s(data); i<n; i++){\n" % _gen_stub_suffix(e, parent)
		out += "\tchild_data = in.get_%s(i, data);\n" % _gen_stub_suffix(e, parent)
		out += utils.indent(_gen_write_element_body())
		out += "}\n"
	elif e.optional:
		out += "if(in.has_%s(data)){\n" % _gen_stub_suffix(e, parent)
		out += "\tchild_data = in.get_%s(data);\n" % _gen_stub_suffix(e, parent)
		out += utils.indent(_gen_write_element_body())
		out += "}\n"
	else:
		out += "child_data = in.get_%s(data);\n" % _gen_stub_suffix(e, parent)
		out += _gen_write_element_body()

	return out

def _gen_write_element(e: UxsdElement, parent: str) -> str:
	"""Function to generate partial C++ code for writing out a struct generated
	from an UxsdElement.

	Currently, all values with non-zero default values are emitted.
	Otherwise, we would have to check against the nonzero value, and the
	check would create a case split for all simple types again.(how to compare
	unions? strings? doubles?)
	"""
	out = ""
	if isinstance(e.type, UxsdSimple):
		if e.many:
			out += "for(size_t i=0, n=in.num_%s(data); i<n; i++){\n" % _gen_stub_suffix(e, parent)
			out += "\tos << \"<%s>\" << %s << \"</%s>\";\n" % (e.name, _gen_write_simple(e, parent), e.name)
			out += "}\n"
		elif e.optional:
			out += "if((bool)%s)\n" % _gen_write_simple(e, parent)
			out += "\tos << \"<%s>\" << %s << \"</%s>\";\n" % (e.name, _gen_write_simple(e, parent), e.name)
		else:
			out += "os << \"<%s>\" << %s << \"</%s>\";\n" % (e.name, _gen_write_simple(e, parent), e.name)
	elif isinstance(e.type, UxsdComplex):
		out += _gen_write_complex_element(e, parent)
	else:
		raise TypeError("Unknown type %s." % e.type)
	return out

def write_fn_from_complex_type(t: UxsdComplex) -> str:
	assert isinstance(t.content, (UxsdDfa, UxsdAll, UxsdLeaf))
	out = ""
	out += "template<class T>\n"
	out += "void write_%s(T &in, std::ostream &os, void *data){\n" % t.name
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		if any(isinstance(e.type, UxsdComplex) for e in t.content.children):
			out += "\tvoid * child_data;\n"
		for e in t.content.children:
			out += utils.indent(_gen_write_element(e, t.name))
	elif isinstance(t.content, UxsdLeaf):
		out += "\tos << in.get_%s_value(data);\n" % t.name
	else:
		out += "\treturn;\n"

	out += "}\n"
	return out

def write_fn_from_root_element(e: UxsdElement) -> str:
	assert isinstance(e.type, UxsdComplex)
	out = ""
	out += "template <class T>\n"
	out += "void write_%s_xml(T &in, std::ostream &os){\n" % e.name
	out += "static_assert(std::is_base_of<%sBase, T>::value, \"Base class not derived from %sBase\");\n" % (utils.to_pascalcase(e.name), utils.to_pascalcase(e.name))
	out += "\tvoid *data = NULL;\n"

	if e.type.attrs:
		out += "\tos << \"<%s\";\n" % e.name
		for a in e.type.attrs:
			out += utils.indent(_gen_write_attr(a, e.name))
	else:
		out += "\tos << \"<%s\";\n" % e.name
	out += "\tos << \">\";\n"
	out += "\twrite_%s(in, os, NULL);\n" % e.type.name
	out += "\tos << \"</%s>\";\n" % e.name

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
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {"

	if schema.enums:
		out += "\n\n/* Enum tokens generated from XSD enumerations. */\n"
		enum_tokens = [tokens_from_enum(t) for t in schema.enums]
		out += "\n".join(enum_tokens)

	out += "\n\n/* Base class for the schema. */\n"
	out += gen_base_class(schema)

	out += "\n/* Declarations for internal load functions for the complex types. */\n"
	load_fn_decls = []
	for t in schema.complex_types:
		load_fn_decls.append("template <class T>")
		load_fn_decls.append("void load_%s(const pugi::xml_node &root, T &out, void *data);" % (t.name))
	out += "\n".join(load_fn_decls)

	out += "\n\n/* Declarations for internal write functions for the complex types. */\n"
	write_fn_decls = []
	for t in schema.complex_types:
		write_fn_decls.append("template <class T>")
		write_fn_decls.append("void write_%s(T &in, std::ostream &os, void *data);" % (t.name))
	out += "\n".join(write_fn_decls)

	out += "\n\n/* Load function for the root element. */\n"
	out += load_fn_from_root_element(schema.root_element)
	out += "\n/* Write function for the root element. */\n"
	out += write_fn_from_root_element(schema.root_element)

	out += "\n\n"
	out += triehash.gen_prelude()

	complex_type_tokens = [tokens_from_complex_type(t) for t in schema.complex_types]
	out += "\n/* Tokens for attribute and node names. */\n"
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

	if schema.enums:
		enum_lookups = [lookup_from_enum(t) for t in schema.enums]
		enum_lexers = [lexer_from_enum(t) for t in schema.enums]
		out += "\n\n/* Lookup tables for enums. */\n"
		out += "\n".join(enum_lookups)
		out += "\n\n/* Lexers(string->token functions) for enums. */\n"
		out += "\n".join(enum_lexers)

	# No need to generate a loader for const char * or enums.
	simple_type_loaders = [load_fn_from_simple_type(t) for t in schema.simple_types if not isinstance(t, (UxsdString, UxsdEnum))]
	complex_type_loaders = [load_fn_from_complex_type(t) for t in schema.complex_types]
	out += "\n\n/* Internal loading functions, which validate and load a PugiXML DOM tree into memory. */\n"
	out += "\n".join(simple_type_loaders)
	out += "\n".join(complex_type_loaders)

	# No need to generate a writer for elements without content.
	complex_type_writers = [write_fn_from_complex_type(t) for t in schema.complex_types if t.content is not None]
	out += "\n\n/* Internal writing functions, which uxsdcxx uses to write out a class. */\n"
	out += "\n".join(complex_type_writers)

	if schema.has_dfa:
		out += cpp_templates.dfa_error_defn
	if schema.has_all:
		out += cpp_templates.all_error_defn
	if schema.has_attr:
		out += cpp_templates.attr_error_defn

	out += "\n\n} /* namespace uxsd */\n"
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

	out += "\n} /* namespace uxsd */\n"
	return out
