from typing import Union, List

from . import cpp_templates, utils, __version__
from .utils import checked
from .third_party import triehash
from .schema import (
	UxsdSchema,
	UxsdComplex,
	UxsdDfa,
	UxsdAll,
	UxsdLeaf,
	UxsdElement,
	UxsdEnum,
	UxsdSimple,
	UxsdString,
	UxsdAtomic,
	UxsdAttribute,
)


def pass_at_init(attr: UxsdAttribute):
	if attr.optional:
		return False

	if attr.type.cpp == "const char *":
		return False

	return True


def _gen_attribute_arg(e: Union[UxsdElement, UxsdAttribute], out: bool = False) -> str:
	if out:
		return "%s * %s" % (e.type.cpp, checked(e.name))
	else:
		return "%s %s" % (e.type.cpp, checked(e.name))


def _gen_required_attribute_arg_list(context_type: str, attrs: List[UxsdAttribute],
									out: bool = False, context: str = "ctx") -> str:
	args = []
	if not out:
		args.append("{context_type} &{context}".format(
			context_type=context_type,
			context=context))

	for attr in sorted(attrs, key=lambda attr: attr.name):
		if pass_at_init(attr):
			args.append(_gen_attribute_arg(attr, out=out))

	return ', '.join(args)


def _gen_context_type(t: UxsdComplex, direction: str) -> str:
	return "typename ContextTypes::{type}{rw}Context".format(
		type=utils.to_pascalcase(t.name),
		rw=direction)


def _gen_virtual_fns(t: UxsdComplex) -> str:
	"""
	Generate virtual functions to interface with an element with a complex type.
	"""
	fields = []

	def _add_field(ret: str, verb: str, what: str, args: str):
		out = "virtual inline {return_type} {verb}_{parent}_{child}({args}) = 0;".format(
			return_type=ret,
			verb=verb,
			parent=t.name,
			child=what,
			args=args)
		fields.append(out)

	def _add_set(e: Union[UxsdElement, UxsdAttribute]):
		assert isinstance(e.type, UxsdSimple)
		_add_field(
			ret="void",
			verb="set",
			what=e.name,
			args="{attrs}, {context_type} &ctx".format(
				attrs=_gen_attribute_arg(e),
				context_type=_gen_context_type(t, "Write")))

	def _add_init(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		_add_field(
			ret=_gen_context_type(e.type, "Write"),
			verb="init",
			what=e.name,
			args=_gen_required_attribute_arg_list(_gen_context_type(t, "Write"), e.type.attrs))
		_add_field(
			ret="void",
			verb="finish",
			what=e.name,
			args="{context_type} &ctx".format(
				context_type=_gen_context_type(e.type, "Write")))

	def _add_add_simple(e: UxsdElement):
		assert isinstance(e.type, UxsdSimple)
		_add_field(
			ret="void",
			verb="preallocate",
			what=e.name,
			args="{context_type} &ctx, size_t size".format(
				context_type=_gen_context_type(t, "Write")))
		_add_field(
			ret="void",
			verb="add",
			what=e.name,
			args="{cpp_type} {cpp_name}, {context_type} &ctx".format(
				cpp_type=e.type.cpp,
				cpp_name=checked(e.name),
				context_type=_gen_context_type(t, "Write")))

	def _add_add_complex(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		_add_field(
			ret="void",
			verb="preallocate",
			what=e.name,
			args="{context_type} &ctx, size_t size".format(
				context_type=_gen_context_type(t, "Write")))
		_add_field(
			ret=_gen_context_type(e.type, "Write"),
			verb="add",
			what=e.name,
			args=_gen_required_attribute_arg_list(_gen_context_type(t, "Write"), e.type.attrs))
		_add_field(
			ret="void",
			verb="finish",
			what=e.name,
			args="{context_type} &ctx".format(
				context_type=_gen_context_type(e.type, "Write")))

	def _add_add(e: UxsdElement):
		if isinstance(e.type, UxsdSimple):
			_add_add_simple(e)
		elif isinstance(e.type, UxsdComplex):
			_add_add_complex(e)
		else:
			raise TypeError(e)

	def _add_get_simple(e: Union[UxsdElement, UxsdAttribute]):
		assert isinstance(e.type, UxsdSimple)
		_add_field(
			ret=e.type.cpp,
			verb="get",
			what=e.name,
			args="{context_type} &ctx".format(
				context_type=_gen_context_type(t, "Read")))

	def _add_get_simple_many(e: UxsdElement):
		assert isinstance(e.type, UxsdSimple)
		_add_field(
			ret=e.type.cpp,
			verb="get",
			what=e.name,
			args="size_t n, {context_type} &ctx".format(
				context_type=_gen_context_type(t, "Read")))

	def _add_get_complex(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		_add_field(
			ret=_gen_context_type(e.type, "Read"),
			verb="get",
			what=e.name,
			args="{context_type} &ctx".format(
				context_type=_gen_context_type(t, "Read")))

	def _add_get_complex_many(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		_add_field(
			ret=_gen_context_type(e.type, "Read"),
			verb="get",
			what=e.name,
			args="size_t n, {context_type} &ctx".format(
				context_type=_gen_context_type(t, "Read")))

	def _add_num(e: UxsdElement):
		_add_field("size_t", "num", e.name, _gen_context_type(t, "Read") + " &ctx")

	def _add_has(e: UxsdElement):
		_add_field("bool", "has", e.name, _gen_context_type(t, "Read") + " &ctx")

	for attr in t.attrs:
		_add_get_simple(attr)
		if not pass_at_init(attr):
			_add_set(attr)

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
					if e.optional:
						_add_has(e)
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
		_add_field(
			ret="void",
			verb="set",
			what="value",
			args="{cpp_type} value, {context_type} &ctx".format(
				cpp_type=t.content.type.cpp,
				context_type=_gen_context_type(t, "Write")))
		_add_field(
			ret=t.content.type.cpp,
			verb="get",
			what="value",
			args="{context_type} &ctx".format(
				context_type=_gen_context_type(t, "Read")))

	out = ""
	out += "/** Generated for complex type \"{name}\":\n".format(
		name=t.name)
	out += utils.to_comment_body(t.source)
	out += "\n*/\n"
	out += "\n".join(fields)
	return out


def gen_base_class(schema: UxsdSchema) -> str:
	"""Generate a C++ base class of a root element."""
	out = ""
	root = schema.root_element
	class_name = utils.to_pascalcase(root.name)
	out += "struct Default{pname}ContextTypes {{\n\t".format(pname=class_name)
	out += "\n\t".join("using {name}ReadContext = void *;".format(
		name=utils.to_pascalcase(x.name))
		for x in schema.complex_types)
	out += "\n\t"
	out += "\n\t".join("using {name}WriteContext = void *;".format(
		name=utils.to_pascalcase(x.name))
		for x in schema.complex_types)
	out += "\n};\n"
	out += "\n"
	out += "template<typename ContextTypes=Default{pname}ContextTypes>\n".format(
		pname=class_name)
	out += "class %sBase {\n" % class_name
	out += "public:\n"
	out += "\tvirtual ~{class_name}Base() {{}}\n".format(
		class_name=class_name)

	out += "\tvirtual void start_load(const std::function<void(const char*)> *report_error) = 0;\n"
	out += "\tvirtual void finish_load() = 0;\n"
	out += "\tvirtual void start_write() = 0;\n"
	out += "\tvirtual void finish_write() = 0;\n"
	out += "\tvirtual void error_encountered(const char * file, int line, const char *message) = 0;\n"

	virtual_fns = [_gen_virtual_fns(x) for x in schema.complex_types]
	out += utils.indent("\n\n".join(virtual_fns))
	out += "\n};\n"
	return out


def tokens_from_enum(t: UxsdEnum) -> str:
	"""Generate C++ enum of token values from an UxsdEnum"""
	out = "\n"
	enum_tokens = ["UXSD_INVALID = 0"]
	enum_tokens += [utils.to_token(x) for x in t.enumeration]
	out += "enum class {name} {{{tokens}}};".format(
		name=t.cpp,
		tokens=", ".join(enum_tokens))
	return out


def lookup_from_enum(t: UxsdEnum) -> str:
	"""Generate C++ lookup table of tokens to strings from an UxsdEnum"""
	out = ""
	lookup_tokens = ["\"UXSD_INVALID\""]
	lookup_tokens += ["\"%s\"" % x for x in t.enumeration]
	out += "constexpr const char *lookup_{name}[] = {{{tokens}}};".format(
		name=t.name,
		tokens=", ".join(lookup_tokens))
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
	out += "inline {enum_type} lex_{enum_type}(const char *in, bool throw_on_invalid, const std::function<void(const char *)> * report_error){{\n".format(  # noqa: E501
		enum_type=t.cpp)

	triehash_alphabet = [(x, "{enum_type}::{token}".format(
		enum_type=t.cpp,
		token=utils.to_token(x)))
		for x in t.enumeration]
	out += utils.indent(triehash.gen_lexer_body(triehash_alphabet))

	out += "\tif(throw_on_invalid)\n"
	out += "\t\tnoreturn_report(report_error, (\"Found unrecognized enum value \" + std::string(in) + \" of {enum_type}.\").c_str());\n".format(  # noqa: E501
		enum_type=t.cpp)
	out += "\treturn {enum_type}::UXSD_INVALID;\n".format(
		enum_type=t.cpp)
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
		out += "enum class gtok_{type} {{{tokens}}};\n".format(
			type=t.cpp,
			tokens=", ".join(enum_tokens))
		out += "constexpr const char *gtok_lookup_{type}[] = {{{lookup_tokens}}};".format(
			type=t.cpp,
			lookup_tokens=", ".join(lookup_tokens))
	if t.attrs:
		enum_tokens = [utils.to_token(x.name) for x in t.attrs]
		lookup_tokens = ["\"%s\"" % x.name for x in t.attrs]
		out += "\nenum class atok_{type} {{{tokens}}};\n".format(
			type=t.cpp,
			tokens=", ".join(enum_tokens))
		out += "constexpr const char *atok_lookup_{type}[] = {{{lookup_tokens}}};\n".format(
			type=t.cpp,
			lookup_tokens=", ".join(lookup_tokens))
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
		out += "inline gtok_{type} lex_node_{type}(const char *in, const std::function<void(const char *)> *report_error){{\n".format(  # noqa: E501
			type=t.cpp)
		triehash_alphabet = [(e.name, "gtok_{type}::{token}".format(
			type=t.cpp,
			token=utils.to_token(e.name)))
			for e in t.content.children]
		out += utils.indent(triehash.gen_lexer_body(triehash_alphabet))
		out += "\tnoreturn_report(report_error, (\"Found unrecognized child \" + std::string(in) + \" of <{name}>.\").c_str());\n".format(  # noqa: E501
			name=t.name)
		out += "}\n"

	if t.attrs:
		out += "inline atok_{type} lex_attr_{type}(const char *in, const std::function<void(const char *)> * report_error){{\n".format(  # noqa: E501
			type=t.cpp)
		triehash_alphabet = [(x.name, "atok_{type}::{token}".format(
			type=t.cpp,
			token=utils.to_token(x.name)))
			for x in t.attrs]
		out += utils.indent(triehash.gen_lexer_body(triehash_alphabet))
		out += "\tnoreturn_report(report_error, (\"Found unrecognized attribute \" + std::string(in) + \" of <{name}>.\").c_str());\n".format(  # noqa: E501
			name=t.name)
		out += "}\n"

	return out


def _gen_dfa_table(t: UxsdComplex) -> str:
	"""Generate a 2D C++ array representing DFA table from an UxsdComplex's DFA.

	The array is indexed by the state and input token value, such that table[state][input]
	gives the next state.
	"""
	assert isinstance(t.content, UxsdDfa)
	dfa = t.content.dfa
	out = ""
	out += "constexpr int NUM_{typeu}_STATES = {num_states};\n".format(
		typeu=t.cpp.upper(),
		num_states=len(dfa.states))
	out += "constexpr const int NUM_{typeu}_INPUTS = {num_inputs};\n".format(
		typeu=t.cpp.upper(),
		num_inputs=len(dfa.alphabet))
	out += "constexpr int gstate_{type}[NUM_{typeu}_STATES][NUM_{typeu}_INPUTS] = {{\n".format(
		type=t.cpp,
		typeu=t.cpp.upper())
	for i in range(0, max(dfa.states) + 1):
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
		return "lex_{type}({input}, true, report_error)".format(
			type=t.cpp,
			input=input)
	else:
		return "load_{type}({input}, report_error)".format(
			type=utils.to_snakecase(t.cpp),
			input=input)


def _gen_load_element_complex(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdComplex)
	out = "{\n"

	args = ["context"]
	load_args = []

	for attr in t.type.attrs:
		if not pass_at_init(attr):
			continue

		arg = "%s_%s" % (t.type.name, checked(attr.name))
		out += "\t%s %s;\n" % (attr.type.cpp, arg)
		out += "\tmemset(&{name}, 0, sizeof({name}));\n".format(name=arg)
		args.append(arg)
		load_args.append('&' + arg)

	if len(load_args) > 0:
		out += "\tload_{type}_required_attributes(node, {load_args}, report_error);\n".format(
			type=t.type.name,
			load_args=', '.join(load_args))
	if t.many:
		out += "\tauto child_context = out.add_{stub}({args});\n".format(
			stub=_gen_stub_suffix(t, parent),
			args=', '.join(args))
	else:
		out += "\tauto child_context = out.init_{stub}({args});\n".format(
			stub=_gen_stub_suffix(t, parent),
			args=', '.join(args))

	out += "\tload_{type}(node, out, child_context, report_error, offset_debug);\n".format(
		type=t.type.name)
	out += "\tout.finish_{stub}(child_context);\n".format(
		stub=_gen_stub_suffix(t, parent))
	out += "}\n"
	return out


def _gen_load_element_simple(t: UxsdElement, parent: str) -> str:
	assert isinstance(t.type, UxsdSimple)
	out = ""
	if t.many:
		out += "out.add_{stub}({loaded_value}, context);\n".format(
			stub=_gen_stub_suffix(t, parent),
			loaded_value=_gen_load_simple(t.type, "node.child_value()"))
	else:
		out += "out.set_{stub}({loaded_value}, context);\n".format(
			stub=_gen_stub_suffix(t, parent),
			loaded_value=_gen_load_simple(t.type, "node.child_value()"))
	return out


def _gen_load_element(t: UxsdElement, parent: str) -> str:
	if isinstance(t.type, UxsdComplex):
		return _gen_load_element_complex(t, parent)
	else:
		return _gen_load_element_simple(t, parent)


def _gen_load_attr(t: UxsdAttribute, parent: str) -> str:
	if not pass_at_init(t):
		return "out.set_{stub}({loaded_value}, context);\n".format(
			stub=_gen_stub_suffix(t, parent),
			loaded_value=_gen_load_simple(t.type, "attr.value()"))
	else:
		return "/* Attribute {attr} is already set */\n".format(
			attr=t.name)


def _gen_load_dfa(t: UxsdComplex) -> str:
	"""Partial function to generate the child element validation&loading section
	of a C++ function load_foo, if the model group is an xs:sequence or xs:choice.

	xs:sequence/xs:choice groups can be compiled down to a finite automaton.
	This is done in dfa.py. A C++ state table is generated in _gen_dfa_table and the
	stream of child elements are validated by running the state machine via the table
	in this function.

	The C++ table has -1s in place of invalid state transitions. If we step into a -1,
	we call dfa_error. We check again at the end of input. If we aren't in an accepted
	state, we again call dfa_error.
	"""
	assert isinstance(t.content, UxsdDfa)
	dfa = t.content.dfa

	any_many = False
	out = ""
	for el in t.content.children:
		if el.many:
			if not any_many:
				out += "// Preallocate arrays by counting child nodes (if any)\n"
			out += "size_t {tag}_count = 0;\n".format(tag=el.name)
			any_many = True

	if any_many:
		out += "{\n"
		out += "\tint next, state={start_state};\n".format(
			start_state=dfa.start)
		out += "\tfor(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()) {\n"
		out += "\t\t*offset_debug = node.offset_debug();\n"
		out += "\t\tgtok_{type} in = lex_node_{type}(node.name(), report_error);\n".format(
			type=t.cpp)

		out += "\t\tnext = gstate_{type}[state][(int)in];\n".format(
			type=t.cpp)
		out += "\t\tif(next == -1)\n"
		out += "\t\t\tdfa_error(gtok_lookup_{type}[(int)in], gstate_{type}[state], gtok_lookup_{type}, {num_of_tokens}, report_error);\n".format(  # noqa: E501
			type=t.cpp,
			num_of_tokens=len(dfa.alphabet))
		out += "\t\tstate = next;\n"

		out += "\t\tswitch(in) {\n"
		for el in t.content.children:
			out += "\t\tcase gtok_{type}::{token}:\n".format(
				type=t.cpp,
				token=utils.to_token(el.name))
			if el.many:
				out += "\t\t\t{tag}_count += 1;\n".format(
					tag=el.name)
			out += "\t\t\tbreak;\n"
		out += "\t\tdefault: break; /* Not possible. */\n"
		out += "\t\t}\n"
		out += "\t}\n"
		out += "\t\n"

		for el in t.content.children:
			if el.many:
				out += "\tout.preallocate_{stub}(context, {tag}_count);\n".format(
					stub=_gen_stub_suffix(el, t.name),
					tag=el.name)

		out += "}\n"

	out += "\tint next, state={start_state};\n".format(
		start_state=dfa.start)
	out += "\tfor(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()) {\n"
	out += "\t\t*offset_debug = node.offset_debug();\n"
	out += "\t\tgtok_{type} in = lex_node_{type}(node.name(), report_error);\n".format(
		type=t.cpp)

	out += "\t\tnext = gstate_{type}[state][(int)in];\n".format(
		type=t.cpp)
	out += "\t\tif(next == -1)\n"
	out += "\t\t\tdfa_error(gtok_lookup_{type}[(int)in], gstate_{type}[state], gtok_lookup_{type}, {num_of_tokens}, report_error);\n".format(  # noqa: E501
		type=t.cpp,
		num_of_tokens=len(dfa.alphabet))
	out += "\t\tstate = next;\n"

	out += "\tswitch(in){\n"
	for el in t.content.children:
		out += "\t\tcase gtok_{type}::{token}:\n".format(
			type=t.cpp,
			token=utils.to_token(el.name))
		out += utils.indent(_gen_load_element(el, t.name), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n"

	reject_cond = " && ".join(["state != {accept}".format(accept=x) for x in dfa.accepts])
	out += "}\n"
	out += "if({reject_cond}) dfa_error(\"end of input\", gstate_{type}[state], gtok_lookup_{type}, {num_of_tokens}, report_error);\n".format(  # noqa: E501
		reject_cond=reject_cond,
		type=t.cpp,
		num_of_tokens=len(dfa.alphabet))

	return out


def _gen_load_all(t: UxsdComplex) -> str:
	"""Partial function to generate the child element validation&loading section
	of a C++ function load_foo, if the model group is an xs:all.

	xs:alls can be validated in a similar fashion to xs:attributes. We maintain a
	bitset of which elements are found. At the end, we OR our bitset with the value
	corresponding to the optional elements and check if all bits in it are set. If not,
	we call attr_error with the token lookup table and the OR'd bitset.
	"""
	assert isinstance(t.content, UxsdAll)
	out = ""

	out += "std::bitset<{num_state_bits}> gstate = 0;\n".format(
		num_state_bits=len(t.content.children))
	out += "for(pugi::xml_node node = root.first_child(); node; node = node.next_sibling()){\n"
	out += "\t*offset_debug = node.offset_debug();\n"
	out += "\tgtok_{type} in = lex_node_{type}(node.name(), report_error);\n".format(
		type=t.cpp)

	out += "\tif(gstate[(int)in] == 0) gstate[(int)in] = 1;\n"
	out += "\telse noreturn_report(report_error, (\"Duplicate element \" + std::string(node.name()) + \" in <{name}>.\").c_str());\n".format(  # noqa: E501
		name=t.name)

	out += "\tswitch(in){\n"
	for el in t.content.children:
		out += "\tcase gtok_{type}::{token}:\n".format(
			type=t.cpp,
			token=utils.to_token(el.name))
		out += utils.indent(_gen_load_element(el, t.name), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n"
	out += "}\n"

	optional_mask = "".join(["1" if x.optional else "0" for x in t.content.children][::-1])
	out += "std::bitset<{num_state_bits}> test_gstate = gstate | std::bitset<{num_state_bits}>(0b{optional_mask});\n".format(  # noqa: E501
		num_state_bits=len(t.content.children),
		optional_mask=optional_mask)
	out += "if(!test_gstate.all()) all_error(test_gstate, gtok_lookup_{type}, report_error);\n".format(  # noqa: E501
		type=t.cpp)

	return out


def _gen_load_required_attrs(t: UxsdComplex) -> str:
	"""
	Generate the required attribute loading section of a C++ function `load_foo`.
	Required attributes are loaded before calling user function `init_foo`
	in order to make initialization easier.
	See `_gen_load_all` to see how attributes are validated.
	"""
	assert len(t.attrs) > 0

	out = ""
	out += "std::bitset<{num_state_bits}> astate = 0;\n".format(
		num_state_bits=len(t.attrs))
	out += "for(pugi::xml_attribute attr = root.first_attribute(); attr; attr = attr.next_attribute()){\n"  # noqa: E501
	out += "\tatok_{type} in = lex_attr_{type}(attr.name(), report_error);\n".format(
		type=t.cpp)
	out += "\tif(astate[(int)in] == 0) astate[(int)in] = 1;\n"
	out += "\telse noreturn_report(report_error, (\"Duplicate attribute \" + std::string(attr.name()) + \" in <{name}>.\").c_str());\n".format(  # noqa: E501
		name=t.name)

	out += "\tswitch(in){\n"
	for attr in t.attrs:
		out += "\tcase atok_{type}::{token}:\n".format(
			type=t.cpp,
			token=utils.to_token(attr.name))
		if pass_at_init(attr):
			out += "\t\t*{attr} = {loaded_value};\n".format(
				attr=checked(attr.name),
				loaded_value=_gen_load_simple(attr.type, "attr.value()"))
		else:
			out += "\t\t/* Attribute {attr} set after element init */\n".format(
				attr=attr.name)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n"
	out += "}\n"

	optional_mask = "".join(["1" if x.optional else "0" for x in t.attrs][::-1])
	out += "std::bitset<{num_state_bits}> test_astate = astate | std::bitset<{num_state_bits}>(0b{optional_mask});\n".format(  # noqa: E501
		num_state_bits=len(t.attrs),
		optional_mask=optional_mask)
	out += "if(!test_astate.all()) attr_error(test_astate, atok_lookup_{type}, report_error);\n".format(  # noqa: E501
		type=t.cpp)
	return out


def _gen_load_attrs(t: UxsdComplex) -> str:
	"""
	Generate the attribute loading section of a C++ function load_foo.
	See _gen_load_all to see how attributes are validated.
	"""

	count = 0
	for attr in t.attrs:
		if not pass_at_init(attr):
			count += 1

	if count == 0:
		# All attributes already handled.
		return ""

	assert len(t.attrs) > 0
	out = ""
	out += "for(pugi::xml_attribute attr = root.first_attribute(); attr; attr = attr.next_attribute()){\n"  # noqa: E501
	out += "\tatok_{type} in = lex_attr_{type}(attr.name(), report_error);\n".format(
		type=t.cpp)

	out += "\tswitch(in){\n"
	for attr in t.attrs:
		out += "\tcase atok_{type}::{token}:\n".format(
			type=t.cpp,
			token=utils.to_token(attr.name))
		out += utils.indent(_gen_load_attr(attr, t.name), 2)
		out += "\t\tbreak;\n"
	out += "\tdefault: break; /* Not possible. */\n"
	out += "\t}\n"
	out += "}\n"

	return out


def load_required_attrs_fn_from_complex_type(t: UxsdComplex) -> str:
	"""
	Generate a C++ function which loads an XSD complex type's required attributes.
	"""
	out = ""
	out += "inline void load_{name}_required_attributes(const pugi::xml_node &root, {required_attrs}, const std::function<void(const char *)> * report_error){{\n".format(  # noqa: E501
		name=t.name,
		required_attrs=_gen_required_attribute_arg_list("", t.attrs, out=True))

	out += utils.indent(_gen_load_required_attrs(t))

	out += "}\n"
	return out


def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""
	Generate a C++ function which loads an XSD complex type into memory.
	"""
	out = ""
	out += "template<class T, typename Context>\n"
	out += "inline void load_{name}(const pugi::xml_node &root, T &out, Context &context, const std::function<void(const char*)> *report_error, ptrdiff_t *offset_debug){{\n".format(  # noqa: E501
		name=t.name)

	out += "\t(void)root;\n"
	out += "\t(void)out;\n"
	out += "\t(void)context;\n"
	out += "\t(void)report_error;\n"
	out += "\t// Update current file offset in case an error is encountered.\n"
	out += "\t*offset_debug = root.offset_debug();\n"
	out += "\n"
	if t.attrs:
		out += utils.indent(_gen_load_attrs(t))
	else:
		out += "\tif(root.first_attribute())\n"
		out += "\t\tnoreturn_report(report_error, \"Unexpected attribute in <%s>.\");\n" % t.name
	out += "\n"

	if isinstance(t.content, UxsdDfa):
		out = _gen_dfa_table(t) + out
		out += utils.indent(_gen_load_dfa(t))
	elif isinstance(t.content, UxsdAll):
		out += utils.indent(_gen_load_all(t))
	elif isinstance(t.content, UxsdLeaf):
		out += "\tout.set_{name}_value({loaded_value}, context);\n".format(
			name=t.name,
			loaded_value=_gen_load_simple(t.content.type, "root.child_value()"))

	if not isinstance(t.content, (UxsdDfa, UxsdAll)):
		out += "\tif(root.first_child().type() == pugi::node_element)\n"
		out += "\t\tnoreturn_report(report_error, \"Unexpected child element in <{name}>.\");\n".format(  # noqa: E501
			name=t.name)
	out += "\n"

	out += "}\n"
	return out


# See https://stackoverflow.com/questions/26080829/detecting-strtol-failure
# Since detecting additional characters require some other hoops which would
# hurt performance, we only check errno.
def load_fn_from_simple_type(t: UxsdSimple) -> str:
	"""Generate a full C++ function load_foo(str)
	which can load an XSD simple type from str and return it.
	"""
	out = ""
	out += "inline {type} load_{type_snake}(const char *in, const std::function<void(const char *)> * report_error){{\n".format(  # noqa: E501
		type=t.cpp,
		type_snake=utils.to_snakecase(t.cpp))
	out += "\t{type} out;\n".format(
		type=t.cpp)
	if isinstance(t, UxsdAtomic):
		out += "\tout = {loaded_atomic};\n".format(
			loaded_atomic=(t.cpp_load_format % "in"))
		out += "\tif(errno != 0)\n"
		out += "\t\tnoreturn_report(report_error, (\"Invalid value `\" + std::string(in) + \"` when loading into a {type}.\").c_str());\n".format(  # noqa: E501
			type=t.cpp)
	elif isinstance(t, UxsdEnum):
		out += "\tout = lex_{type}(in, true);\n".format(
			type=t.cpp)
	else:
		raise TypeError("Unsupported simple type %s." % t)
	out += "\treturn out;\n"
	out += "}\n"
	return out


def load_fn_from_root_element(e: UxsdElement) -> str:
	return cpp_templates.load_root_element_defn.format(
		name=e.name,
		type=e.type.name)


def _gen_get_simple(t: Union[UxsdElement, UxsdAttribute],
						parent: str, context: str = "context") -> str:
	assert isinstance(t.type, UxsdSimple)
	if isinstance(t.type, UxsdAtomic):
		if isinstance(t, UxsdElement) and t.many:
			return "in.get_{stub}(i, {context})".format(
				stub=_gen_stub_suffix(t, parent),
				context=context)
		else:
			return "in.get_{stub}({context})".format(
				stub=_gen_stub_suffix(t, parent),
				context=context)
	elif isinstance(t.type, UxsdEnum):
		if isinstance(t, UxsdElement) and t.many:
			return "lookup_{type}[(int)in.get_{stub}(i, {context})]".format(
				type=t.type.name,
				stub=_gen_stub_suffix(t, parent),
				context=context)
		else:
			return "lookup_{type}[(int)in.get_{stub}({context})]".format(
				type=t.type.name,
				stub=_gen_stub_suffix(t, parent),
				context=context)
	else:
		raise NotImplementedError(t)


def _gen_write_attr(a: UxsdAttribute, parent: str, context: str = "context") -> str:
	"""
	Generate partial code which writes out a single XML attribute.
	"""
	out = ""
	if not a.optional or a.default_value:
		out += "os << \" {attr}=\\\"\" << {value} << \"\\\"\";\n".format(
			attr=a.name,
			value=_gen_get_simple(a, parent, context))
	else:
		out += "if((bool){value})\n".format(
			value=_gen_get_simple(a, parent, context))
		out += "\tos << \" {attr}=\\\"\" << {value} << \"\\\"\";\n".format(
			attr=a.name,
			value=_gen_get_simple(a, parent, context))
	return out


def _gen_write_complex_element(e: UxsdElement, parent: str) -> str:
	"""
	Generate partial code which writes out an element with a complex type.
	"""
	assert isinstance(e.type, UxsdComplex)
	out = ""

	def _gen_write_element_body() -> str:
		assert isinstance(e.type, UxsdComplex)
		ouv = ""
		if e.type.attrs:
			ouv += "os << \"<{name}\";\n".format(
				name=e.name)
			for a in e.type.attrs:
				ouv += _gen_write_attr(a, e.type.name, "child_context")
			if e.type.content:
				ouv += "os << \">\";\n"
				ouv += "write_{type}(in, os, child_context);\n".format(
					type=e.type.name)
				ouv += "os << \"</{name}>\\n\";\n".format(
					name=e.name)
			else:
				ouv += "os << \"/>\\n\";\n"
		else:
			if e.type.content:
				ouv += "os << \"<{name}>\\n\";\n".format(
					name=e.name)
				ouv += "write_{type}(in, os, child_context);\n".format(
					type=e.type.name)
				ouv += "os << \"</{name}>\\n\";\n".format(
					name=e.name)
			else:
				ouv += "os << \"<{name}/>\\n\";\n".format(
					name=e.name)
		return ouv

	if e.many:
		out += "for(size_t i=0, n=in.num_{stub}(context); i<n; i++){{\n".format(
			stub=_gen_stub_suffix(e, parent))
		out += "\tauto child_context = in.get_{stub}(i, context);\n".format(
			stub=_gen_stub_suffix(e, parent))
		out += utils.indent(_gen_write_element_body())
		out += "}\n"
	elif e.optional:
		out += "if(in.has_{stub}(context)){{\n".format(
			stub=_gen_stub_suffix(e, parent))
		out += "\tauto child_context = in.get_{stub}(context);\n".format(
			stub=_gen_stub_suffix(e, parent))
		out += utils.indent(_gen_write_element_body())
		out += "}\n"
	else:
		out += "auto child_context = in.get_{stub}(context);\n".format(
			stub=_gen_stub_suffix(e, parent))
		out += _gen_write_element_body()

	return out


def _gen_write_element(e: UxsdElement, parent: str) -> str:
	"""
	Generate partial code to write out an element.

	Currently, all values with non-zero default values are emitted.
	Otherwise, we would have to check against the nonzero value, and the
	check would create a case split for all simple types again.(how to compare
	unions? strings? doubles?)
	"""
	out = ""
	if isinstance(e.type, UxsdSimple):
		if e.many:
			out += "for(size_t i=0, n=in.num_{stub}(context); i<n; i++){{\n".format(
				stub=_gen_stub_suffix(e, parent))
			out += "\tos << \"<{name}>\" << {value} << \"</{name}>\\n\";\n".format(
				name=e.name,
				value=_gen_get_simple(e, parent))
			out += "}\n"
		elif e.optional:
			out += "if((bool){value})\n".format(
				value=_gen_get_simple(e, parent))
			out += "\tos << \"<{name}>\" << {value} << \"</{name}>\\n\";\n".format(
				name=e.name,
				value=_gen_get_simple(e, parent))
		else:
			out += "\tos << \"<{name}>\" << {value} << \"</{name}>\\n\";\n".format(
				name=e.name,
				value=_gen_get_simple(e, parent))
	elif isinstance(e.type, UxsdComplex):
		out += "{\n"
		out += utils.indent(_gen_write_complex_element(e, parent))
		out += "}\n"
	else:
		raise TypeError("Unknown type %s." % e.type)
	return out


def write_fn_from_complex_type(t: UxsdComplex) -> str:
	assert isinstance(t.content, (UxsdDfa, UxsdAll, UxsdLeaf))
	out = ""
	out += "template<class T, typename Context>\n"
	out += "inline void write_{name}(T &in, std::ostream &os, Context &context){{\n".format(
		name=t.name)
	out += "\t(void)in;\n"
	out += "\t(void)os;\n"
	out += "\t(void)context;\n"
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for e in t.content.children:
			out += utils.indent(_gen_write_element(e, t.name))
	elif isinstance(t.content, UxsdLeaf):
		out += "\tos << in.get_{name}_value(context);\n".format(
			name=t.name)
	else:
		out += "\treturn;\n"

	out += "}\n"
	return out


def write_fn_from_root_element(e: UxsdElement) -> str:
	assert isinstance(e.type, UxsdComplex)
	out = ""
	out += "template <class T, typename Context>\n"
	out += "inline void write_{name}_xml(T &in, Context &context, std::ostream &os){{\n".format(
		name=e.name)
	out += "\tin.start_write();\n"

	if e.type.attrs:
		out += "\tos << \"<{name}\";\n".format(
			name=e.name)
		for a in e.type.attrs:
			out += utils.indent(_gen_write_attr(a, e.name))
	else:
		out += "\tos << \"<{name}\";\n".format(
			name=e.name)

	out += "\tos << \">\\n\";\n"
	out += "\twrite_{content_type}(in, os, context);\n".format(
		content_type=e.type.name)
	out += "\tos << \"</{name}>\\n\";\n".format(
		name=e.name)
	out += "\tin.finish_write();\n"

	out += "}\n"
	return out


def render_interface_header_file(schema: UxsdSchema, cmdline: str, input_file: str) -> str:
	"""Render a C++ header file to a string."""
	out = ""
	out += cpp_templates.header_comment.format(
		version=__version__,
		cmdline=cmdline,
		input_file=input_file,
		md5=utils.md5(input_file))
	out += "\n"
	out += "#include <cstdlib>\n"
	out += "#include <functional>\n"
	out += "#include <tuple>\n"
	out += "\n"
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {"

	if schema.enums:
		out += "\n\n/* Enum tokens generated from XSD enumerations. */\n"
		enum_tokens = [tokens_from_enum(t) for t in schema.enums]
		out += "\n".join(enum_tokens)

	out += "\n\n/* Base class for the schema. */\n"
	out += gen_base_class(schema)
	out += "\n} /* namespace uxsd */\n"

	return out


def render_header_file(schema: UxsdSchema, cmdline: str, input_file: str,
						interface_header_file_name: str) -> str:
	"""Render a C++ header file to a string."""
	out = ""
	out += cpp_templates.header_comment.format(
		version=__version__,
		cmdline=cmdline,
		input_file=input_file,
		md5=utils.md5(input_file))
	out += cpp_templates.includes
	out += '#include "{}"'.format(interface_header_file_name)
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {\n"

	out += cpp_templates.get_line_number_decl
	out += cpp_templates.report_error_decl

	out += "\n/* Declarations for internal load functions for the complex types. */\n"
	load_fn_decls = []
	for t in schema.complex_types:
		load_fn_decls.append(cpp_templates.load_complex_type_decl.format(
			name=t.name))
		if sum(pass_at_init(attr) for attr in t.attrs) > 0:
			load_fn_decls.append(cpp_templates.load_required_attrs_decl.format(
				name=t.name,
				args=_gen_required_attribute_arg_list("", t.attrs, out=True)))
	out += "".join(load_fn_decls)

	out += "\n\n/* Declarations for internal write functions for the complex types. */\n"
	write_fn_decls = []
	for t in schema.complex_types:
		if t.content is None:
			continue
		write_fn_decls.append(cpp_templates.write_complex_type_decl.format(
			name=t.name))
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
	simple_type_loaders = [load_fn_from_simple_type(t)
							for t in schema.simple_types
							if not isinstance(t, (UxsdString, UxsdEnum))]
	complex_type_attr_loaders = [load_required_attrs_fn_from_complex_type(t)
								for t in schema.complex_types
								if sum(pass_at_init(attr) for attr in t.attrs) > 0]
	complex_type_loaders = [load_fn_from_complex_type(t)
							for t in schema.complex_types]
	out += "\n\n/* Internal loading functions, which validate and load\n"
	out += " * a PugiXML DOM tree into memory. */\n"
	out += "\n".join(simple_type_loaders)
	out += "\n".join(complex_type_attr_loaders)
	out += "\n".join(complex_type_loaders)

	# No need to generate a writer for elements without content.
	complex_type_writers = [write_fn_from_complex_type(t)
							for t in schema.complex_types
							if t.content is not None]
	out += "\n\n/* Internal writing functions, which uxsdcxx uses to write out a class. */\n"
	out += "\n".join(complex_type_writers)

	if schema.has_dfa:
		out += cpp_templates.dfa_error_defn
	if schema.has_all:
		out += cpp_templates.all_error_defn
	if schema.has_attr:
		out += cpp_templates.attr_error_defn
	out += cpp_templates.get_line_number_defn

	out += "\n\n} /* namespace uxsd */\n"
	return out
