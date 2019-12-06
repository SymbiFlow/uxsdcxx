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
	out += "inline enum_{name} conv_enum_{name}(ucap::{pname} e) {{\n".format(
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
	out += "inline void load_%s_capnp(T &out, kj::ArrayPtr<const ::capnp::word> data){\n" % e.name
	out += "\tstatic_assert(std::is_base_of<%sBase, T>::value, \"Base class not derived from %sBase\");\n" % (
			utils.to_pascalcase(e.name),
			utils.to_pascalcase(e.name))

	out += "\t/* Increase reader limit to 1G words. */\n"
	out += "\t::capnp::ReaderOptions opts = ::capnp::ReaderOptions();\n"
	out += "\topts.traversalLimitInWords = 1024 * 1024 * 1024;\n"
	out += "\t::capnp::FlatArrayMessageReader reader(data, opts);\n"
	out += "\tauto root = reader.getRoot<ucap::{}>();\n".format(to_type(e))
	out += "\n"
	out += "\tload_{}_capnp_type(root, out, nullptr, nullptr);\n".format(e.name);
	out += "}\n"
	return out


def load_fn_from_complex_type(t: UxsdComplex) -> str:
	"""Generate a full C++ function load_foo(&root, &out)
	which can load an XSD complex type from DOM &root into C++ object out.
	"""
	out = ""
	out += "template<class T>\n"
	out += "inline void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, const void *data, void *iter){{\n".format(
			name=t.name,
			cname=utils.to_pascalcase(t.name))

	out += "\t(void)root;\n"
	out += "\t(void)out;\n"
	out += "\t(void)data;\n"
	out += "\t(void)iter;\n"
	out += "\n"
	for attr in t.attrs:
		if cpp.pass_at_init(attr):
			continue

		out += "\tout.set_{suffix}({data}, data, iter);\n".format(
			suffix=cpp._gen_stub_suffix(attr, t.name),
            data=_gen_load_simple(attr.type, 'root.get{pname}()'.format(
                pname=utils.to_pascalcase(attr.name))))

	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = utils.to_pascalcase(el.name)
			if el.many:
				out += "\tfor(const auto & el : root.get{pname}()) {{\n".format(
						pname=utils.pluralize(name))
				out += "\t\tconst void *child_data;\n"
				out += "\t\tvoid *child_iter;\n"
				out += "\t\t(void)child_data;\n"
				out += "\t\t(void)iter;\n"
				if isinstance(el.type, UxsdComplex):
					out += "\t\tstd::tie(child_data, child_iter) = out.add_{suffix}(data, iter{required_attrs});\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							required_attrs=_gen_required_attribute_arg_list(el, 'el'))
					out += "\t\tload_{suffix}_capnp_type(el, out, child_data, child_iter);\n".format(
							suffix=el.type.name)
					out += "\t\tout.finish_{suffix}(child_data, child_iter);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name))
				else:
					out += "\t\tout.add_{suffix}({data}, data, iter);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							data=_gen_load_simple(el, 'el.get{}()'.format(name))
							)
				out += "\t}\n"
			else:
				out += "\tif (root.has{pname}()) {{\n".format(pname=name)
				out += "\t\tconst void *child_data;\n"
				out += "\t\tvoid *child_iter;\n"
				out += "\t\t(void)child_data;\n"
				out += "\t\t(void)iter;\n"
				if isinstance(el.type, UxsdComplex):
					access = 'root.get{pname}()'.format(pname=name)
					out += "\t\tstd::tie(child_data, child_iter) = out.init_{suffix}(data, iter{required_attrs});\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							required_attrs=_gen_required_attribute_arg_list(el, access))
					out += "\t\tload_{suffix}_capnp_type({access}, out, child_data, child_iter);\n".format(
							access=access,
							suffix=el.type.name,
							pname=name)
					out += "\t\tout.finish_{suffix}(child_data, child_iter);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name))
				else:
					out += "\t\tout.init_{suffix}({data}, data, iter);\n".format(
							suffix=cpp._gen_stub_suffix(el, t.name),
							data=_gen_load_simple(el, 'root.get{name}()'.format(name)))
				out += "\t}\n"
	elif isinstance(t.content, UxsdLeaf):
		out += "\tout.set_{name}_value(root.getValue().cStr(), data, iter);\n".format(
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
		load_fn_decls.append("void load_{name}_capnp_type(const ucap::{cname}::Reader &root, T &out, const void *data, void *iter);".format(
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


def _gen_capnp_impl(t: UxsdComplex, is_root : bool) -> str:
	fields = []

	def _get_builder():
		if is_root:
			return "auto *builder = &{}_builder_;\n".format(t.name)
		else:
			return "auto *builder = static_cast<ucap::{}::Builder*>(iter);\n".format(utils.to_pascalcase(t.name))

	def _get_reader():
		if is_root:
			return "const auto *reader = &{}_reader_;\n".format(t.name)
		else:
			return "const auto *reader = static_cast<const ucap::{}::Reader*>(iter);\n".format(utils.to_pascalcase(t.name))

	def _add_field(ret: str, verb: str, what: str, args: str, impl: str):
		fields.append("inline %s %s_%s_%s(%s) override {\n%s}\n" % (ret, verb, t.name, what, args, utils.indent(impl)))

	def _gen_set_required_attrs(e: UxsdElement):
		impl = ""
		for attr in sorted(e.type.attrs, key=lambda attr: attr.name):
			if cpp.pass_at_init(attr):
				impl += '{ename}_builder_.set{pname}({value});\n'.format(
						ename=e.type.name,
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

		impl = ""
		impl += "auto *builder = static_cast<ucap::{}::Builder*>(iter);\n".format(utils.to_pascalcase(e.type.name))
		for el in e.type.content.children:
			if el.many:
				impl += "auto {cname} = builder->init{pname}({name}_.size());\n".format(
						cname=cpp.checked(el.name),
						name=utils.pluralize(el.type.name),
						pname=utils.to_pascalcase(utils.pluralize(el.name)))
				impl += "for(size_t i = 0; i < {name}_.size(); ++i) {{\n".format(
						name=utils.pluralize(el.type.name))
				impl += "\t{cname}.adoptWithCaveats(i, std::move({name}_[i]));\n".format(
						cname=cpp.checked(el.name),
						name=utils.pluralize(el.type.name))
				impl += "}\n"
				impl += "{name}_.clear();\n".format(name=utils.pluralize(el.type.name))
		return impl

		any_many = False
		for e2 in e.content.children:
			if isinstance(e2.type, UxsdComplex):
				if e2.many:
					any_many = True
					break
			elif isinstance(e2.type, UxsdSimple):
				if e2.many:
					any_many = True
					break
			else:
				raise TypeError(e2)

		if not any_many:
			return ""

		impl = ""
		impl += _get_builder()

		return impl

	def _add_set(e: Union[UxsdElement, UxsdAttribute]):
		impl = ""
		impl += _get_builder()
		impl += "builder->set{pname}({value});\n".format(
				pname=utils.to_pascalcase(e.name),
				value=_gen_set_simple(e.type, e.name))
		_add_field("void", "set", e.name, cpp._gen_attribute_arg(e) + ", const void * data, void * iter", impl)

	def _add_init(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += _get_builder()
		impl += "{ename}_builder_ = builder->init{pname}();\n".format(
				ename=e.type.name,
				pname=utils.to_pascalcase(e.name))
		impl += _gen_set_required_attrs(e)
		impl += "return std::make_pair(nullptr, &{}_builder_);\n".format(e.type.name)
		_add_field("std::pair<const void *, void *>", "init", e.name, cpp._gen_required_attribute_arg_list(e.type.attrs), impl)

		_add_field("void", "finish", e.name, "const void * data, void * iter", _gen_finish(e))

	def _add_add_simple(e: UxsdElement):
		impl = ""
		_add_field("void", "add", e.name, "%s %s, const void * data, void * iter" % (e.type.cpp, cpp.checked(e.name)), impl)

	def _add_add_complex(e: UxsdElement):
		assert isinstance(e.type, UxsdComplex)
		impl = ""
		impl += _get_builder()
		impl += "auto {name} = capnp::Orphanage::getForMessageContaining(*builder).newOrphan<ucap::{pname}>();\n".format(
				name=cpp.checked(e.name), pname=utils.to_pascalcase(e.type.name))
		impl += "{pname}_.emplace_back(std::move({name}));\n".format(name=cpp.checked(e.name), pname=utils.pluralize(e.type.name))
		impl += "{name}_builder_ = {pname}_.back().get();\n".format(name=e.type.name, pname=utils.pluralize(e.type.name))
		impl += _gen_set_required_attrs(e)
		impl += "return std::make_pair(nullptr, &{name}_builder_);\n".format(name=e.type.name)
		_add_field("std::pair<const void *, void *>", "add", e.name, cpp._gen_required_attribute_arg_list(e.type.attrs), impl)

		_add_field("void", "finish", e.name, "const void * data, void * iter", _gen_finish(e))

	def _add_add(e: UxsdElement):
		if isinstance(e.type, UxsdSimple): _add_add_simple(e)
		elif isinstance(e.type, UxsdComplex): _add_add_complex(e)
		else: raise TypeError(e)

	def _add_get_simple(e: Union[UxsdElement, UxsdAttribute]):
		impl = ""
		impl += _get_reader()
		impl += "return {value};\n".format(value=_gen_load_simple(
			e.type,
			"reader->get{pname}()".format(
				pname=utils.to_pascalcase(e.name))))

		_add_field(e.type.cpp, "get", e.name, "const void * data, void * iter", impl)

	def _add_get_simple_many(e: UxsdElement):
		_add_field(e.type.cpp, "get", e.name, "int n, const void * data, void * iter", "")

	def _add_get_complex(e: UxsdElement):
		impl = ""
		impl += _get_reader()
		impl += "{name}_reader_ = reader->get{pname}();\n".format(name=e.type.name, pname=utils.to_pascalcase(e.name))
		impl += "return std::make_pair(nullptr, &{name}_reader_);\n".format(name=e.type.name)
		_add_field("std::pair<const void *, void *>", "get", e.name, "const void * data, void * iter", impl)

	def _add_get_complex_many(e: UxsdElement):
		impl = ""
		impl += _get_reader()
		impl += "{name}_reader_ = reader->get{pname}()[n];\n".format(name=e.type.name, pname=utils.to_pascalcase(utils.pluralize(e.name)))
		impl += "return std::make_pair(nullptr, &{name}_reader_);\n".format(name=e.type.name)
		_add_field("std::pair<const void *, void *>", "get", e.name, "int n, const void * data, void * iter", impl)

	def _add_num(e: UxsdElement):
		impl = ""
		impl += _get_reader()
		impl += "return reader->get{pname}().size();\n".format(pname=utils.to_pascalcase(utils.pluralize(e.name)))
		_add_field("size_t", "num", e.name, "const void * data, void * iter", impl)

	def _add_has(e: UxsdElement):
		impl = ""
		impl += _get_reader()
		if e.many:
			pname = utils.to_pascalcase(utils.pluralize(e.name))
		else:
			pname = utils.to_pascalcase(e.name)
		impl += "return reader->has{pname}();\n".format(pname=pname)
		_add_field("bool", "has", e.name, "const void * data, void * iter", impl)

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
		impl = ""
		impl += "auto *{var} = static_cast<ucap::{pname}::Builder*>(iter);\n".format(
				var=cpp.checked(t.name),
				pname=utils.to_pascalcase(t.name))
		impl += "{var}->setValue(value);\n".format(
				var=cpp.checked(t.name))
		_add_field("void", "set", "value", "%s value, const void * data, void *iter" % t.content.type.cpp, impl)

		impl = ""
		impl += "const auto *{var} = static_cast<const ucap::{pname}::Reader*>(data);\n".format(
				var=cpp.checked(t.name),
				pname=utils.to_pascalcase(t.name))
		impl += "return {value};\n".format(value=_gen_load_simple(
			t.content.type,
			"{var}->getValue()".format(
				var=cpp.checked(t.name))))
		_add_field(t.content.type.cpp, "get", "value", "const void * data, void *iter", impl)

	return '\n'.join(fields)

def render_impl_header_file(schema: UxsdSchema, cmdline: str, capnp_file_name: str, interface_file_name: str, input_file: str) -> str:
	out = ""
	x = {"version": __version__,
		"cmdline": cmdline,
		"input_file": input_file,
		"md5": utils.md5(input_file)}
	out += cpp_templates.header_comment.substitute(x)
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
	out += "class Capnp{pname} : public {pname}Base {{\n".format(
			pname=pname)
	out += "public:\n"
	out += "\tCapnp{pname}() :\n\t".format(pname=pname)
	out += "\t,".join(" {name}_builder_(nullptr)\n".format(name=t.name) for t in schema.complex_types)
	out += "\t\t{}\n\n"
	out += "\tvoid set_{name}_builder(ucap::{pname}::Builder builder) {{\n".format(
			name=schema.root_element.name,
			pname=utils.to_pascalcase(schema.root_element.name))
	out += "\t\t{}_builder_ = builder;\n".format(schema.root_element.name)
	out += "\t}\n\n"
	out += "\tvoid set_{name}_reader(ucap::{pname}::Reader reader) {{\n".format(
			name=schema.root_element.name,
			pname=utils.to_pascalcase(schema.root_element.name))
	out += "\t\t{}_reader_ = reader;\n".format(schema.root_element.name)
	out += "\t}\n\n"
	for t in schema.complex_types:
		out += utils.indent(_gen_capnp_impl(t, t.name == schema.root_element.name))
	out += "private:\n"
	for t in schema.complex_types:
		out += "\tucap::{pname}::Builder {name}_builder_;\n".format(
				pname=utils.to_pascalcase(t.name),
				name=t.name)
		out += "\tucap::{pname}::Reader {name}_reader_;\n\n".format(
				pname=utils.to_pascalcase(t.name),
				name=t.name)

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
