from typing import Union
from random import getrandbits
from . import cpp_templates, cpp

from uxsdcxx import capnp_templates as tpl
from uxsdcxx import utils, __version__
from uxsdcxx.schema import (
	UxsdSchema,
	UxsdAttribute,
	UxsdType,
	UxsdElement,
	UxsdComplex,
	UxsdUnion,
	UxsdEnum,
	UxsdAtomic,
	UxsdAll,
	UxsdDfa,
	UxsdLeaf,
	UxsdSimple,
	UxsdString,
)


def gen_file_id() -> str:
	"""Cap'n Proto IDs are 64 bits and the first bit is always 1."""
	return "@%s;" % hex(getrandbits(64) | (1 << 63))


def gen_namespace() -> str:
	"""Generate Cap'n Proto namespace. It's always named `ucap`."""
	out = "using Cxx = import \"/capnp/c++.capnp\";\n"
	out += "$Cxx.namespace(\"ucap\");"
	return out


def to_type(t: UxsdType) -> str:
	"""For determining type in fields only."""
	if isinstance(t, UxsdAtomic):
		return tpl.atomic_builtins[t.name]
	else:
		return utils.to_pascalcase(t.name)


def complex_to_capnp(t: UxsdComplex) -> str:
	fields = []
	i = 0
	for attr in t.attrs:
		name = utils.to_camelcase(attr.name)
		type = to_type(attr.type)
		if attr.default_value is not None:
			field = "\t{name} @{i} :{type} = {default_value};".format(
				name=name, i=i, type=type, default_value=attr.default_value)
		else:
			field = "\t{name} @{i} :{type};".format(
				name=name, i=i, type=type)
		fields.append(field)
		i += 1
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = utils.to_camelcase(el.name)
			type = to_type(el.type)
			if el.many:
				field = "\t{pname} @{i} :List({type});".format(
					pname=utils.pluralize(name), i=i, type=type)
				i += 1
			else:
				field = "\t%{name} @%{i} :{type};".format(
					name=name, i=i, type=type)
				i += 1
			fields.append(field)
	elif isinstance(t.content, UxsdLeaf):
		field = "\tvalue @{i} :{type};".format(
			i=i, type=to_type(t.content.type))
		fields.append(field)
	out = "struct {type} {{\n".format(
		type=type)
	out += "\n".join(fields)
	out += "\n}"
	return out


def enum_to_capnp(t: UxsdEnum) -> str:
	fields = ['\tuxsdInvalid @0;']
	for i, e in enumerate(t.enumeration):
		fields.append("\t{enum} @{i};".format(
			enum=utils.to_camelcase(e), i=i+1))  # noqa: E226
	out = ""
	out += "enum {type} {{\n".format(
		type=to_type(t))
	out += "\n".join(fields)
	out += "\n}"
	return out


def _gen_conv_enum(t: UxsdEnum) -> str:
	pname = utils.to_pascalcase(t.name)
	out = ""
	out += "inline enum_{name} conv_enum_{name}(ucap::{pname} e, const std::function<void(const char *)> * report_error) {{\n".format(  # noqa: E501
		name=t.name,
		pname=pname)
	out += "\tswitch(e) {\n"
	out += "\tcase ucap::{pname}::{e}:\n".format(
		pname=pname,
		e='UXSD_INVALID')
	out += "\t\treturn enum_{name}::{e};\n".format(
		name=t.name,
		e='UXSD_INVALID')
	for e in t.enumeration:
		out += "\tcase ucap::{pname}::{e}:\n".format(
			pname=pname,
			e=e.upper())
		out += "\t\treturn enum_{name}::{e};\n".format(
			name=t.name,
			e=e.upper())
	out += "\tdefault:\n"
	out += '\t\t(*report_error)("Unknown enum_{name}");\n'.format(name=t.name)
	out += "\t\tthrow std::runtime_error(\"Unreachable!\");\n"
	out += "\t}\n"
	out += "}\n"
	out += "\n"
	out += "inline ucap::{pname} conv_to_enum_{name}(enum_{name} e) {{\n".format(
		name=t.name,
		pname=pname)
	out += "\tswitch(e) {\n"
	out += "\tcase enum_{name}::{e}:\n".format(
		name=t.name,
		e='UXSD_INVALID')
	out += "\t\treturn ucap::{pname}::{e};\n".format(
		pname=pname,
		e='UXSD_INVALID')
	for e in t.enumeration:
		out += "\tcase enum_{name}::{e}:\n".format(
			name=t.name,
			e=e.upper())
		out += "\t\treturn ucap::{pname}::{e};\n".format(
			pname=pname,
			e=e.upper())
	out += "\tdefault:\n"
	out += '\t\tthrow std::runtime_error("Unknown enum_{name}");\n'.format(name=t.name)
	out += "\t}\n"
	out += "}\n"

	return out


def _gen_required_attribute_arg_list(t: UxsdComplex, input: str) -> str:
	required_attrs = []

	for attr in sorted(t.attrs, key=lambda attr: attr.name):
		if cpp.pass_at_init(attr):
			required_attrs.append(_gen_load_simple(
				attr.type,
				input='{input}.get{pname}()'.format(
					input=input,
					pname=utils.to_pascalcase(attr.name))))

	if len(required_attrs) == 0:
		return ""
	else:
		return ', ' + ', '.join(required_attrs)


def _gen_load_simple(t: UxsdSimple, input: str) -> str:
	if isinstance(t, UxsdString):
		return input + '.cStr()'
	elif isinstance(t, UxsdEnum):
		return 'conv_enum_{type}({input}, report_error)'.format(
			type=t.name,
			input=input)
	else:
		return input


def _gen_set_simple(t: UxsdSimple, input: str) -> str:
	if isinstance(t, UxsdString):
		return input
	elif isinstance(t, UxsdEnum):
		return 'conv_to_enum_{type}({input})'.format(
			type=t.name,
			input=input)
	else:
		return input


def union_to_capnp(t: UxsdUnion) -> str:
	"""Declare global unnamed union inside struct."""
	fields = []
	for i, m in enumerate(t.member_types):
		name = utils.to_camelcase(m.name)
		field = "\t\t%s @%d :%s;" % (name, i, to_type(m))
		fields.append(field)
	out = ""
	out += "struct %s {\n" % to_type(t)
	out += "\tunion {\n"
	out += "\n".join(fields)
	out += "\n\t}"
	out += "\n}"
	return out


def render_capnp_file(schema: UxsdSchema, cmdline: str, input_file: str) -> str:
	out = ""
	x = {
		"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)
	}
	out += tpl.header_comment.substitute(x)
	out += "\n\n"
	out += gen_file_id()
	out += "\n"
	out += gen_namespace()
	enums = [enum_to_capnp(e) for e in schema.enums]
	if enums:
		out += "\n\n"
		out += "\n\n".join(enums)
	unions = [union_to_capnp(t) for t in schema.unions]
	if unions:
		out += "\n\n"
		out += "\n\n".join(unions)
	complexes = [complex_to_capnp(t) for t in schema.complex_types]
	out += "\n\n"
	out += "\n\n".join(complexes)
	out += "\n"
	return out


def load_fn_from_element(e: UxsdElement) -> str:
	# Initial memory allocation for capnp stack for error reporting.
	INITIAL_STACK_DEPTH = 20
	out = ""
	out += "template <class T, typename Context>\n"
	out += "inline void load_{name}_capnp(T &out, kj::ArrayPtr<const ::capnp::word> data, Context &context, const char * filename){{\n".format(  # noqa: E501
		name=e.name)

	out += "\t/* Remove traversal limits. */\n"
	out += "\t::capnp::ReaderOptions opts = ::capnp::ReaderOptions();\n"
	out += "\topts.traversalLimitInWords = std::numeric_limits<uint64_t>::max();\n"
	out += "\t::capnp::FlatArrayMessageReader reader(data, opts);\n"
	out += "\tauto root = reader.getRoot<ucap::{}>();\n".format(to_type(e.type))
	out += "\tstd::vector<std::pair<const char*, size_t>> stack;\n"
	out += "\tstack.reserve({});\n".format(INITIAL_STACK_DEPTH)
	out += "\tstack.push_back(std::make_pair(\"root\", 0));\n"
	out += "\n"
	out += "\tstd::function<void(const char *)> report_error = [filename, &out, &stack](const char *message){\n"  # noqa: E501
	out += "\t\tstd::stringstream msg;\n"
	out += "\t\tmsg << message << std::endl;\n"
	out += "\t\tmsg << \"Error occured at \";\n"
	out += "\t\tfor(size_t i = 0; i < stack.size(); ++i) {\n"
	out += "\t\t\tmsg << stack[i].first << \"[\" << stack[i].second << \"]\";\n"
	out += "\t\t\tif(i+1 < stack.size()) {\n"
	out += "\t\t\t\tmsg << \" . \";\n"
	out += "\t\t\t}\n"
	out += "\t\t}\n"

	out += "\t\tout.error_encountered(filename, -1, msg.str().c_str());\n"
	out += "\t};\n"
	out += "\tout.start_load(&report_error);\n"
	out += "\tload_{name}_capnp_type(root, out, context, &report_error, &stack);\n".format(
		name=e.name)
	out += "\tout.finish_load();\n"
	out += "}\n"
	return out


def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""
	Generate a full C++ function which can load a complex type from Cap'n Proto.
	"""
	out = ""
	out += "template<class T, typename Context>\n"
	out += "inline void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, Context &context, const std::function<void(const char*)> * report_error, std::vector<std::pair<const char *, size_t>> * stack){{\n".format(  # noqa: E501
		name=t.name,
		cname=utils.to_pascalcase(t.name))

	out += "\t(void)root;\n"
	out += "\t(void)out;\n"
	out += "\t(void)context;\n"
	out += "\t(void)report_error;\n"
	out += "\t(void)stack;\n"
	out += "\n"
	for attr in t.attrs:
		if cpp.pass_at_init(attr):
			continue

		out += "\tout.set_{suffix}({data}, context);\n".format(
			suffix=cpp._gen_stub_suffix(attr, t.name),
			data=_gen_load_simple(attr.type, 'root.get{pname}()'.format(
				pname=utils.to_pascalcase(attr.name))))

	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = utils.to_pascalcase(el.name)
			out += "\tstack->push_back(std::make_pair(\"get{}\", 0));\n".format(name)
			if el.many:
				out += "\t{\n"
				suffix = cpp._gen_stub_suffix(el, t.name)
				out += "\t\tauto data = root.get{pname}();\n".format(pname=utils.pluralize(name))
				out += "\t\tout.preallocate_{suffix}(context, data.size());\n".format(suffix=suffix)

				out += "\t\tfor(const auto & el : data) {\n"
				if isinstance(el.type, UxsdComplex):
					out += "\t\t\tauto child_context = out.add_{suffix}(context{required_attrs});\n".format(  # noqa: E501
						suffix=suffix,
						required_attrs=_gen_required_attribute_arg_list(el.type, 'el'))
					out += "\t\t\tload_{suffix}_capnp_type(el, out, child_context, report_error, stack);\n".format(  # noqa: E501
						suffix=el.type.name)
					out += "\t\t\tout.finish_{suffix}(child_context);\n".format(
						suffix=suffix)
				elif isinstance(el.type, UxsdSimple):
					out += "\t\t\tout.add_{suffix}({data}, context);\n".format(
						suffix=suffix,
						data=_gen_load_simple(el.type, 'el.get{}()'.format(name)))
				else:
					raise TypeError("Element %s's type is neither complex nor simple." % el)
				out += "\t\t\tstack->back().second += 1;\n"
				out += "\t\t}\n"
				out += "\t}\n"
			else:
				out += "\tif (root.has{pname}()) {{\n".format(pname=name)
				if isinstance(el.type, UxsdComplex):
					out += "\t\tauto child_el = root.get{pname}();\n".format(pname=name)
					access = 'child_el'
					out += "\t\tauto child_context = out.init_{suffix}(context{required_attrs});\n".format(
						suffix=cpp._gen_stub_suffix(el, t.name),
						required_attrs=_gen_required_attribute_arg_list(el.type, access))
					out += "\t\tload_{suffix}_capnp_type({access}, out, child_context, report_error, stack);\n".format(  # noqa: E501
						access=access,
						suffix=el.type.name)
					out += "\t\tout.finish_{suffix}(child_context);\n".format(
						suffix=cpp._gen_stub_suffix(el, t.name))
				elif isinstance(el.type, UxsdSimple):
					out += "\t\tout.init_{suffix}({data}, context);\n".format(
						suffix=cpp._gen_stub_suffix(el, t.name),
						data=_gen_load_simple(el.type, 'root.get{name}()'.format(name=name)))
				else:
					raise TypeError("Element %s's type is neither complex nor simple." % el)
				out += "\t}\n"

			out += "\tstack->pop_back();\n"
	elif isinstance(t.content, UxsdLeaf):
		out += "\tstack->push_back(std::make_pair(\"getValue\", 0));\n"
		out += "\tout.set_{name}_value(root.getValue().cStr(), context);\n".format(
			name=t.name)
		out += "\tstack->pop_back();\n"

	out += "}\n"
	return out


def _gen_write_simple(t: Union[UxsdElement, UxsdAttribute],
						parent: str, context: str = "context") -> str:
	if isinstance(t.type, UxsdAtomic):
		if isinstance(t, UxsdElement) and t.many:
			return "in.get_%s(i, %s)" % (cpp._gen_stub_suffix(t, parent), context)
		else:
			return "in.get_%s(%s)" % (cpp._gen_stub_suffix(t, parent), context)
	elif isinstance(t.type, UxsdEnum):
		if isinstance(t, UxsdElement) and t.many:
			return _gen_set_simple(
				t.type,
				input="in.get_{name}(i, {context})".format(
					name=cpp._gen_stub_suffix(t, parent),
					context=context))
		else:
			return _gen_set_simple(
				t.type,
				input="in.get_{name}({context})".format(
					name=cpp._gen_stub_suffix(t, parent),
					context=context))
	else:
		raise NotImplementedError(t)


def _gen_write_complex_element(e: UxsdElement, parent: str) -> str:
	"""Function to generate partial code which writes out an element with a complex type."""
	assert isinstance(e.type, UxsdComplex)
	out = ""

	def _gen_write_element_body(root: str) -> str:
		assert isinstance(e.type, UxsdComplex)
		ouv = ""
		if e.type.attrs:
			for a in e.type.attrs:
				ouv += _gen_write_attr(a, e.type.name, root, "child_context")
			if e.type.content:
				ouv += "write_{name}_capnp_type(in, {root}, child_context);\n".format(
					root=root,
					name=e.type.name)
		else:
			if e.type.content:
				ouv += "write_{name}_capnp_type(in, {root}, child_context);\n".format(
					root=root,
					name=e.type.name)

		return ouv

	name = cpp._gen_stub_suffix(e, parent)
	if e.many:
		plural_name = utils.pluralize(cpp._gen_stub_suffix(e, parent))
		out += "size_t num_{pname} = in.num_{name}(context);\n".format(name=name, pname=plural_name)
		out += "auto {name} = root.init{pname}(num_{name});\n".format(
			name=plural_name,
			pname=utils.pluralize(utils.to_pascalcase(e.name)))
		out += "for(size_t i = 0; i < num_{name}; i++) {{\n".format(
			name=plural_name)
		out += "\tauto {name} = {plural_name}[i];\n".format(
			name=name,
			plural_name=plural_name)
		out += "\tauto child_context = in.get_{stub}(i, context);\n".format(
			stub=cpp._gen_stub_suffix(e, parent))
		out += utils.indent(_gen_write_element_body(name))
		out += "}\n"
	elif e.optional:
		out += "if(in.has_{stub}(context)){{\n".format(
			stub=cpp._gen_stub_suffix(e, parent))
		out += "\tauto {name} = root.init{pname}();\n".format(
			name=name,
			pname=utils.to_pascalcase(e.name))
		out += "\tauto child_context = in.get_{stub}(context);\n".format(
			stub=cpp._gen_stub_suffix(e, parent))
		out += utils.indent(_gen_write_element_body(name))
		out += "}\n"
	else:
		out += "{\n"
		out += "\tauto child_context = in.get_{stub}(context);\n".format(
			stub=cpp._gen_stub_suffix(e, parent))
		out += "\tauto {name} = root.init{pname}();\n".format(
			name=name,
			pname=utils.to_pascalcase(e.name))
		out += utils.indent(_gen_write_element_body(name))
		out += "}\n"

	return out


def _gen_write_element(e: UxsdElement, parent: str) -> str:
	"""Function to generate partial C++ code for writing out a struct generated
	from an UxsdElement.

	Currently, all values with non-zero default values are emitted.
	Otherwise, we would have to check against the nonzero value, and the
	check would create a case split for all simple types again.(how to compare
	unions? strings? doubles?)
	"""

	assert isinstance(e.type, UxsdComplex)
	out = ""
	out += "\n"
	out += _gen_write_complex_element(e, parent)

	return out


def write_fn_from_complex_type(t: UxsdComplex) -> str:
	assert isinstance(t.content, (UxsdDfa, UxsdAll, UxsdLeaf))
	out = ""
	out += "template<class T, typename Context>\n"
	out += "inline void write_{name}_capnp_type(T &in, ucap::{cname}::Builder &root, Context &context) {{\n".format(  # noqa: E501
		name=t.name,
		cname=utils.to_pascalcase(t.name))
	out += "\t(void)in;\n"
	out += "\t(void)root;\n"
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for e in t.content.children:
			out += utils.indent(_gen_write_element(e, t.name))
	elif isinstance(t.content, UxsdLeaf):
		out += "\troot.setValue(in.get_{name}_value(context));\n".format(
			name=t.name)
	else:
		out += "\treturn;\n"

	out += "}\n"
	return out


def _gen_check_simple(t: Union[UxsdElement, UxsdAttribute],
						parent: str, context: str = "context") -> str:
	if isinstance(t, UxsdElement) and t.many:
		return "in.get_%s(i, %s)" % (cpp._gen_stub_suffix(t, parent), context)
	else:
		return "in.get_%s(%s)" % (cpp._gen_stub_suffix(t, parent), context)


def _gen_write_attr(a: UxsdAttribute, parent: str, root: str = "root",
					context: str = "context") -> str:
	""" Generate partial code which writes out a single XML attribute."""
	out = ""
	if not a.optional or a.default_value:
		# root.set{pname}(in.get_{}(context);
		out += "{root}.set{pname}({value});\n".format(
			root=root,
			pname=utils.to_pascalcase(a.name),
			value=_gen_write_simple(a, parent, context))
	else:
		out += "if((bool)%s)\n" % _gen_check_simple(a, parent, context)
		out += "\t{root}.set{pname}({value});\n".format(
			root=root,
			pname=utils.to_pascalcase(a.name),
			value=_gen_write_simple(a, parent, context))

	return out


def write_fn_from_root_element(e: UxsdElement) -> str:
	assert isinstance(e.type, UxsdComplex)
	out = ""
	out += "template <class T, typename Context>\n"
	out += "inline void write_{name}_capnp(T &in, Context &context, ucap::{cname}::Builder &root) {{\n".format(  # noqa: E501
		name=e.name,
		cname=utils.to_pascalcase(e.name))

	out += "\tin.start_write();\n"

	if e.type.attrs:
		for a in e.type.attrs:
			out += utils.indent(_gen_write_attr(a, e.name))
	out += "\twrite_{name}_capnp_type(in, root, context);\n".format(
		name=e.name)

	out += "\tin.finish_write();\n"

	out += "}\n"
	return out


def render_header_file(schema: UxsdSchema, cmdline: str, capnp_file_name: str,
						interface_file_name: str, input_file: str) -> str:
	"""Render a C++ header file to a string."""
	out = ""
	out += cpp_templates.header_comment.format(
		version=__version__,
		cmdline=cmdline,
		input_file=input_file,
		md5=utils.md5(input_file))
	out += '#include <stdexcept>\n'
	out += '#include <tuple>\n'
	out += '#include <vector>\n'
	out += '#include <sstream>\n'
	out += '#include <limits>\n'
	out += '#include "capnp/serialize.h"\n'
	out += '#include "{capnp_file_name}.h"\n'.format(
		capnp_file_name=capnp_file_name)
	out += '#include "{interface_file_name}"\n'.format(
		interface_file_name=interface_file_name)
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {"

	out += "\n/* Declarations for internal load functions for the complex types. */\n"
	load_fn_decls = []
	for t in schema.complex_types:
		load_fn_decls.append("template <class T, typename Context>")
		load_fn_decls.append("void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, Context &context, const std::function<void(const char*)> * report_error, std::vector<std::pair<const char *, size_t>> * stack);".format(  # noqa: E501
			name=t.name,
			cname=utils.to_pascalcase(t.name)))
	out += "\n".join(load_fn_decls)

	out += "\n\n/* Declarations for internal write functions for the complex types. */\n"
	write_fn_decls = []
	for t in schema.complex_types:
		if t.content is None:
			continue
		write_fn_decls.append("template <class T, typename Context>")
		write_fn_decls.append("inline void write_{name}_capnp_type(T &in, ucap::{cname}::Builder &root, Context &context);".format(  # noqa: E501
			name=t.name,
			cname=utils.to_pascalcase(t.name)))
	out += "\n".join(write_fn_decls)

	if schema.enums:
		enum_converters = [_gen_conv_enum(t) for t in schema.enums]
		out += "\n\n/* Enum conversions from uxsd to ucap */\n"
		out += "\n".join(enum_converters)

	out += "\n\n/* Load function for the root element. */\n"
	out += load_fn_from_element(schema.root_element)
	out += "\n/* Write function for the root element. */\n"
	out += write_fn_from_root_element(schema.root_element)

	complex_type_loaders = [load_fn_from_complex_type(t) for t in schema.complex_types]

	out += "\n\n/* Internal loading functions, which load a Cap'n Proto message. */\n"
	out += "\n".join(complex_type_loaders)

	complex_type_writers = [write_fn_from_complex_type(t)
							for t in schema.complex_types
							if t.content is not None]
	out += "\n\n/* Internal writing functions, which uxsdcxx uses to write out a class. */\n"
	out += "\n".join(complex_type_writers)

	out += "\n\n} /* namespace uxsd */\n"
	return out


def _gen_reader(e: UxsdComplex):
	return 'ucap::{}::Reader'.format(utils.to_pascalcase(e.name))


def _gen_builder(e: UxsdComplex):
	return 'ucap::{}::Builder'.format(utils.to_pascalcase(e.name))


def _gen_capnp_impl(t: UxsdComplex, is_root: bool) -> str:
	fields = []

	def _add_field(ret: str, verb: str, what: str, args: str, impl: str):
		out = "inline {return_type} {verb}_{parent}_{child}({args}) override {{\n{impl}}}\n".format(
			return_type=ret,
			verb=verb,
			parent=t.name,
			child=what,
			args=args,
			impl=utils.indent(impl))
		fields.append(out)

	def _gen_set_required_attrs(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		for attr in sorted(e.type.attrs, key=lambda attr: attr.name):
			if cpp.pass_at_init(attr):
				impl += 'child_builder.set{pname}({value});\n'.format(
					pname=utils.to_pascalcase(attr.name),
					value=_gen_set_simple(attr.type, attr.name))
		return impl

	def _gen_finish(e: UxsdElement):
		any_many = False
		if not isinstance(e.type, UxsdComplex):
			return ""

		if isinstance(e.type.content, (UxsdDfa, UxsdAll)):
			for el in e.type.content.children:
				if el.many:
					any_many = True
					break

		if not any_many:
			return ""

		# This is guaranteed at this point, but mypy can't keep track
		assert isinstance(e.type.content, (UxsdDfa, UxsdAll))
		impl = ""
		for el in e.type.content.children:
			if el.many:
				impl += "auto {cname} = builder.init{pname}({name}_.size());\n".format(
					cname=cpp.checked(el.name),
					name=utils.pluralize(el.type.name),
					pname=utils.to_pascalcase(utils.pluralize(el.name)))
				impl += "for(size_t i = 0; i < {name}_.size(); ++i) {{\n".format(
					name=utils.pluralize(el.type.name))
				impl += "\t{cname}.adoptWithCaveats(i, std::move({name}_[i]));\n".format(
					cname=cpp.checked(el.name),
					name=utils.pluralize(el.type.name))
				impl += "}\n"
				impl += "{name}_.clear();\n".format(
					name=utils.pluralize(el.type.name))

		return impl

	def _add_set(e: Union[UxsdElement, UxsdAttribute]):
		assert isinstance(e.type, UxsdSimple)
		_add_field(
			ret="void",
			verb="set",
			what=e.name,
			args="{attrs}, {builder_type} &builder".format(
				attrs=cpp._gen_attribute_arg(e),
				builder_type=_gen_builder(t)),
			impl="builder.set{pname}({value});\n".format(
				pname=utils.to_pascalcase(e.name),
				value=_gen_set_simple(e.type, e.name)))

	def _add_init(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += "auto child_builder = builder.init{pname}();\n".format(
			pname=utils.to_pascalcase(e.name))
		impl += _gen_set_required_attrs(e)
		impl += "return child_builder;\n"
		_add_field(
			ret=_gen_builder(e.type),
			verb="init",
			what=e.name,
			args=cpp._gen_required_attribute_arg_list(
				_gen_builder(t), e.type.attrs, context="builder"),
			impl=impl)
		_add_field(
			ret="void",
			verb="finish",
			what=e.name,
			args=_gen_builder(e.type) + " &builder",
			impl=_gen_finish(e))

	def _add_add_simple(e: UxsdElement):
		_add_field(
			ret="void",
			verb="add",
			what=e.name,
			args="{cpp_type} {cpp_name}, {builder_type} &builder".format(
				cpp_type=e.type.cpp,
				cpp_name=cpp.checked(e.name),
				builder_type=_gen_builder(t)),
			impl="")

	def _add_add_complex(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += "auto {name} = capnp::Orphanage::getForMessageContaining(builder).newOrphan<ucap::{pname}>();\n".format(  # noqa: E501
			name=cpp.checked(e.name),
			pname=utils.to_pascalcase(e.type.name))
		impl += "{pname}_.emplace_back(std::move({name}));\n".format(
			name=cpp.checked(e.name),
			pname=utils.pluralize(e.type.name))
		impl += "auto child_builder = {pname}_.back().get();\n".format(
			pname=utils.pluralize(e.type.name))
		impl += _gen_set_required_attrs(e)
		impl += "return child_builder;\n"

		_add_field(
			ret=_gen_builder(e.type),
			verb="add",
			what=e.name,
			args=cpp._gen_required_attribute_arg_list(
				_gen_builder(t), e.type.attrs, context="builder"),
			impl=impl)
		_add_field(
			ret="void",
			verb="preallocate",
			what=e.name,
			args="{builder_type}&, size_t size".format(
				builder_type=_gen_builder(t)),
			impl="{pname}_.reserve(size);\n".format(
				pname=utils.pluralize(e.type.name)))
		_add_field(
			ret="void",
			verb="finish",
			what=e.name,
			args=_gen_builder(e.type) + " & builder",
			impl=_gen_finish(e))

	def _add_add(e: UxsdElement):
		if isinstance(e.type, UxsdSimple):
			_add_add_simple(e)
		elif isinstance(e.type, UxsdComplex):
			_add_add_complex(e)
		else:
			raise TypeError(e)

	def _add_get_simple(e: Union[UxsdElement, UxsdAttribute]):
		assert isinstance(e.type, UxsdSimple)
		impl = ""
		impl += "return {value};\n".format(value=_gen_load_simple(
			e.type,
			"reader.get{pname}()".format(
				pname=utils.to_pascalcase(e.name))))

		_add_field(e.type.cpp, "get", e.name, _gen_reader(t) + " &reader", impl)

	def _add_get_simple_many(e: UxsdElement):
		_add_field(e.type.cpp, "get", e.name, "int n, {} &reader".format(_gen_reader(t)), "")

	def _add_get_complex(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += "return reader.get{pname}();\n".format(pname=utils.to_pascalcase(e.name))
		_add_field(_gen_reader(e.type), "get", e.name, _gen_reader(t) + " &reader", impl)

	def _add_get_complex_many(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += "return reader.get{pname}()[n];\n".format(
			pname=utils.to_pascalcase(utils.pluralize(e.name)))
		_add_field(_gen_reader(e.type), "get", e.name, "int n, {} &reader".format(_gen_reader(t)), impl)

	def _add_num(e: UxsdElement):
		impl = ""
		impl += "return reader.get{pname}().size();\n".format(
			pname=utils.to_pascalcase(utils.pluralize(e.name)))
		_add_field("size_t", "num", e.name, _gen_reader(t) + " &reader", impl)

	def _add_has(e: UxsdElement):
		impl = ""
		if e.many:
			pname = utils.to_pascalcase(utils.pluralize(e.name))
		else:
			pname = utils.to_pascalcase(e.name)
		impl += "return reader.has{pname}();\n".format(pname=pname)
		_add_field("bool", "has", e.name, _gen_reader(t) + " &reader", impl)

	for attr in t.attrs:
		_add_get_simple(attr)
		if not cpp.pass_at_init(attr):
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
			args="{cpp_type} value, {builder_type} &builder".format(
				cpp_type=t.content.type.cpp,
				builder_type=_gen_builder(t)),
			impl="builder.setValue(value);\n")
		_add_field(
			ret=t.content.type.cpp,
			verb="get",
			what="value",
			args="{reader} &reader".format(
				reader=_gen_reader(t)),
			impl="return {value};\n".format(
				value=_gen_load_simple(t.content.type, "reader.getValue()")))

	return '\n'.join(fields)


def render_impl_header_file(schema: UxsdSchema, cmdline: str, capnp_file_name: str,
							interface_file_name: str, input_file: str) -> str:
	out = ""
	out += cpp_templates.header_comment.format(
		version=__version__,
		cmdline=cmdline,
		input_file=input_file,
		md5=utils.md5(input_file))
	out += '#include <stdexcept>\n'
	out += '#include <vector>\n'
	out += '#include "capnp/serialize.h"\n'
	out += '#include "{}.h"\n'.format(capnp_file_name)
	out += '#include "{}"\n'.format(interface_file_name)
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {\n"

	if schema.enums:
		enum_converters = [_gen_conv_enum(t) for t in schema.enums]
		out += "\n\n/* Enum conversions from uxsd to ucap */\n"
		out += "\n".join(enum_converters)

	pname = utils.to_pascalcase(schema.root_element.name)
	out += "struct Capnp{pname}ContextTypes : public Default{pname}ContextTypes {{\n\t".format(
		pname=pname)
	out += "\n\t".join("using {pname}ReadContext = ucap::{pname}::Reader;".format(
		pname=utils.to_pascalcase(t.name))
		for t in schema.complex_types)
	out += "\n\t"
	out += "\n\t".join("using {pname}WriteContext = ucap::{pname}::Builder;".format(
		pname=utils.to_pascalcase(t.name))
		for t in schema.complex_types)
	out += "\n};\n"
	out += "\n"

	out += "class Capnp{pname} : public {pname}Base<Capnp{pname}ContextTypes> {{\n\t".format(
		pname=pname)
	out += "public:\n"
	out += "\tCapnp{pname}() {{}}\n\n".format(pname=pname)

	out += "\tvoid start_load(const std::function<void(const char *)> *report_error_in) override {\n"
	out += "\t\treport_error = report_error_in;\n"
	out += "\t}\n"
	out += "\tvoid finish_load() override {}\n"
	out += "\tvoid start_write() override {}\n"
	out += "\tvoid finish_write() override {}\n"
	out += "\tvoid error_encountered(const char * file, int line, const char *message) override {\n"
	out += "\t\tstd::stringstream msg;"
	out += "\t\tmsg << message << \" occured at file: \" << file << \" line: \" << line;\n"
	out += "\t\tthrow std::runtime_error(msg.str());\n"
	out += "\t}\n"

	for t in schema.complex_types:
		out += utils.indent(_gen_capnp_impl(t, t.name == schema.root_element.name))
	out += "private:\n"
	out += "\tconst std::function<void(const char *)> *report_error;\n"

	for t in schema.complex_types:
		if isinstance(t.content, (UxsdDfa, UxsdAll)):
			for el in t.content.children:
				if el.many:
					out += "\tstd::vector<capnp::Orphan<ucap::{pname}>> {name}_;\n".format(
						pname=utils.to_pascalcase(el.type.name),
						name=utils.pluralize(el.type.name))
	out += "};\n"

	out += "\n\n} /* namespace uxsd */\n"
	return out
