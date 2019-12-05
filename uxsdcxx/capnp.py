from random import getrandbits
from . import cpp_templates, cpp

from uxsdcxx import capnp_templates as tpl
from uxsdcxx import utils, __version__
from uxsdcxx.schema import (
	UxsdSchema,
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
			field = "\t%s @%d :%s = %s;" % (name, i,  type, attr.default_value)
		else:
			field = "\t%s @%d :%s;" % (name, i, type)
		fields.append(field)
		i += 1
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = utils.to_camelcase(el.name)
			type = to_type(el.type)
			if el.many:
				field = "\t%s @%d :List(%s);" % (utils.pluralize(name), i, type)
				i += 1
			else:
				field = "\t%s @%d :%s;" % (name, i, type)
				i += 1
			fields.append(field)
	elif isinstance(t.content, UxsdLeaf):
		field = "\tvalue @%d :%s;" % (i, to_type(t.content.type))
		fields.append(field)
	out = "struct %s {\n" % to_type(t)
	out += "\n".join(fields)
	out += "\n}"
	return out

def enum_to_capnp(t: UxsdEnum) -> str:
	fields = ['\tuxsdInvalid @0;']
	for i, e in enumerate(t.enumeration): # hehe
		fields.append("\t%s @%d;" % (utils.to_camelcase(e), i+1))
	out = ""
	out += "enum %s {\n" % to_type(t)
	out += "\n".join(fields)
	out += "\n}"
	return out

def _gen_conv_enum(t: UxsdEnum) -> str:
	pname = utils.to_pascalcase(t.name)
	out = ""
	out += "enum_{name} conv_enum_{name}(ucap::{pname} e) {{\n".format(
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
	out += '\t\tthrow std::runtime_error("Unknown enum_{name}");\n'.format(name=t.name)
	out += "\t}\n"
	out += "}\n"

	return out

def _gen_required_attribute_arg_list(t: UxsdComplex, input: str) -> str:
	required_attrs = []

	for attr in sorted(t.type.attrs, key=lambda attr: attr.name):
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
		return 'conv_enum_{type}({input})'.format(
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
	x = {"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)}
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
	out = ""
	out += "template <class T>\n"
	out += "void load_%s_capnp(T &out, kj::ArrayPtr<const ::capnp::word> data){\n" % e.name
	out += "\tstatic_assert(std::is_base_of<%sBase, T>::value, \"Base class not derived from %sBase\");\n" % (
			utils.to_pascalcase(e.name),
			utils.to_pascalcase(e.name))

	out += "\t/* Increase reader limit to 1G words. */\n"
	out += "\t::capnp::ReaderOptions opts = ::capnp::ReaderOptions();\n"
	out += "\topts.traversalLimitInWords = 1024 * 1024 * 1024;\n"
	out += "\t::capnp::FlatArrayMessageReader reader(data, opts);\n"
	out += "\tauto root = reader.getRoot<ucap::{}>();\n".format(to_type(e))
	out += "\n"
	out += "\tload_{}_capnp_type(root, out, nullptr);\n".format(e.name);
	out += "}\n"
	return out


def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""Generate a full C++ function load_foo(&root, &out)
	which can load an XSD complex type from DOM &root into C++ object out.
	"""
	out = ""
	out += "template<class T>\n"
	out += "void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, void *data){{\n".format(
			name=t.name,
			cname=utils.to_pascalcase(t.name))

	out += "\t(void)root;\n"
	out += "\t(void)out;\n"
	out += "\t(void)data;\n"
	out += "\n"
	for attr in t.attrs:
		if cpp.pass_at_init(attr):
			continue

		out += "\tout.set_{suffix}({data}, data);\n".format(
			suffix=cpp._gen_stub_suffix(attr, t.name),
            data=_gen_load_simple(attr.type, 'root.get{pname}()'.format(
                pname=utils.to_pascalcase(attr.name))))

	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = utils.to_pascalcase(el.name)
			if el.many:
				out += "\tfor(const auto & el : root.get{pname}()) {{\n".format(
						pname=utils.pluralize(name))
				if isinstance(el.type, UxsdComplex):
					out += "\t\tvoid *child_data = out.add_{suffix}(data{required_attrs});\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							required_attrs=_gen_required_attribute_arg_list(el, 'el'))
					out += "\t\tload_{suffix}_capnp_type(el, out, child_data);\n".format(
							suffix=el.type.name)
					out += "\t\tout.finish_{suffix}(child_data);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name))
				else:
					out += "\t\tout.add_{suffix}({data}, data);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							data=_gen_load_simple(el, 'el.get{}()'.format(name))
							)
				out += "\t}\n"
			else:
				out += "\tif (root.has{pname}()) {{\n".format(pname=name)
				if isinstance(el.type, UxsdComplex):
					access = 'root.get{pname}()'.format(pname=name)
					out += "\t\tvoid *child_data = out.init_{suffix}(data{required_attrs});\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							required_attrs=_gen_required_attribute_arg_list(el, access))
					out += "\t\tload_{suffix}_capnp_type({access}, out, child_data);\n".format(
                            access=access,
							suffix=el.type.name,
							pname=name)
					out += "\t\tout.finish_{suffix}(child_data);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name))
				else:
					out += "\t\tout.init_{suffix}({data}, data);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							data=_gen_load_simple(el, 'root.get{name}()'.format(name)))
				out += "\t}\n"
	elif isinstance(t.content, UxsdLeaf):
		out += "\tout.set_{name}_value(root.getValue().cStr(), data);\n".format(
				name=t.name)

	out += "}\n"
	return out


def render_header_file(schema: UxsdSchema, cmdline: str, capnp_file_name: str, interface_file_name: str, input_file: str) -> str:
	"""Render a C++ header file to a string."""
	out = ""
	x = {"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)}
	out += cpp_templates.header_comment.substitute(x)
	out += '#include <stdexcept>\n'
	out += '#include "capnp/serialize.h"\n'
	out += '#include "{}.h"\n'.format(capnp_file_name)
	out += '#include "{}"\n'.format(interface_file_name)
	out += "\n/* All uxsdcxx functions and structs live in this namespace. */\n"
	out += "namespace uxsd {"

	out += "\n/* Declarations for internal load functions for the complex types. */\n"
	load_fn_decls = []
	for t in schema.complex_types:
		load_fn_decls.append("template <class T>")
		load_fn_decls.append("void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, void *data);".format(
			name=t.name,
			cname=utils.to_pascalcase(t.name)))
	out += "\n".join(load_fn_decls)

	if schema.enums:
		enum_converters = [_gen_conv_enum(t) for t in schema.enums]
		out += "\n\n/* Enum conversions from uxsd to ucap */\n"
		out += "\n".join(enum_converters)

	out += "\n\n/* Load function for the root element. */\n"
	out += load_fn_from_element(schema.root_element)

	complex_type_loaders = [load_fn_from_complex_type(t) for t in schema.complex_types]

	out += "\n\n/* Internal loading functions, which validate and load a PugiXML DOM tree into memory. */\n"
	out += "\n".join(complex_type_loaders)

	out += "\n\n} /* namespace uxsd */\n"
	return out

