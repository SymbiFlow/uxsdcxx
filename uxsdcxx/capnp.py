import sys
import os
import xmlschema # type: ignore

from random import getrandbits
from typing import Any, List, Tuple, Dict, Set, Union, Optional

from uxsdcxx import capnp_templates as tpl
from uxsdcxx import utils, __version__
from uxsdcxx.schema import (
	UxsdSchema,
	UxsdType,
	UxsdSimple,
	UxsdComplex,
	UxsdUnion,
	UxsdEnum,
	UxsdAtomic,
	UxsdAttribute,
	UxsdElement,
	UxsdAll,
	UxsdDfa,
	UxsdLeaf,
)

def gen_file_id() -> str:
	"""Cap'n Proto IDs are 64 bits and the first bit is always 1."""
	return "@%s;" % hex(getrandbits(64) | (1 << 63))

def gen_namespace() -> str:
	out = "using Cxx = import \"/capnp/c++.capnp\";\n"
	out += "$Cxx.namespace(\"ucap\");"
	return out

def to_capnpcase(x: str) -> str:
	x = utils.to_token(x) # normalize
	y = [w[0] + w[1:].lower() for w in x.split("_")]
	return "".join(y)

def to_camelcase(x: str) -> str:
	x = to_capnpcase(x)
	x = x[0].lower() + x[1:]
	return x

def to_type(t: UxsdType) -> str:
	"""For determining type in fields only."""
	if isinstance(t, UxsdAtomic):
		return tpl.atomic_builtins[t.name]
	else:
		return to_capnpcase(t.name)

def complex_to_capnp(t: UxsdComplex) -> str:
	fields = []
	i = 0
	for attr in t.attrs:
		name = to_camelcase(attr.name)
		type = to_type(attr.type)
		if attr.default_value is not None:
			field = "\t%s @%d :%s = %s;" % (name, i,  type, attr.default_value)
		else:
			field = "\t%s @%d :%s;" % (name, i, type)
		fields.append(field)
		i += 1
	if isinstance(t.content, (UxsdDfa, UxsdAll)):
		for el in t.content.children:
			name = to_camelcase(el.name)
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
	fields = []
	for i, e in enumerate(t.enumeration): # hehe
		fields.append("\t%s @%d;" % (to_camelcase(e), i))
	out = ""
	out += "enum %s {\n" % to_type(t)
	out += "\n".join(fields)
	out += "\n}"
	return out

def union_to_capnp(t: UxsdUnion) -> str:
	"""Declare global unnamed union inside struct."""
	fields = []
	for i, m in enumerate(t.member_types):
		name = to_camelcase(m.name)
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