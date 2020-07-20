#!/usr/bin/env python3

import os
import sys

import xmlschema # type: ignore
from uxsdcxx import capnp, cpp
from uxsdcxx.schema import UxsdSchema


def uxsdcxx() -> None:
	"""
	Entry point for uxsdcxx command.
	"""
	input_file = os.path.abspath(sys.argv[1])
	base = os.path.splitext(os.path.basename(input_file))[0]
	interface_header_file_name = base + "_uxsdcxx_interface.h"
	header_file_name = base + "_uxsdcxx.h"
	impl_file_name = base + "_uxsdcxx.cpp"
	cmdline = " ".join(sys.argv)
	schema = UxsdSchema(xmlschema.validators.XMLSchema10(input_file))
	interface_header_file = open(interface_header_file_name, "w")
	interface_header_file.write(cpp.render_interface_header_file(schema, cmdline, input_file))
	interface_header_file.close()
	header_file = open(header_file_name, "w")
	header_file.write(cpp.render_header_file(schema, cmdline, input_file, interface_header_file_name))
	header_file.close()
	impl_file= open(impl_file_name, "w")
	impl_file.write(cpp.render_impl_file(schema, cmdline, input_file, header_file_name))
	impl_file.close()


def uxsdcap() -> None:
	"""
	Entry point for uxsdcap command.
	"""
	input_file = os.path.abspath(sys.argv[1])
	base = os.path.splitext(os.path.basename(input_file))[0]
	capnp_file_name = base + "_uxsdcxx.capnp"
	capnp_header_file_name = base + "_uxsdcxx_capnp.h"
	capnp_header_impl_file_name = base + "_uxsdcxx_capnp_impl.h"
	interface_file_name = base + "_uxsdcxx_interface.h"
	cmdline = " ".join(sys.argv)
	schema = UxsdSchema(xmlschema.XMLSchema(input_file))
	with open(capnp_file_name, "w") as capnp_file:
		capnp_file.write(capnp.render_capnp_file(schema, cmdline, input_file))

	with open(capnp_header_file_name, "w") as header_file:
		header_file.write(capnp.render_header_file(schema, cmdline, capnp_file_name, interface_file_name, input_file))

	with open(capnp_header_impl_file_name, "w") as header_impl_file:
		header_impl_file.write(capnp.render_impl_header_file(schema, cmdline, capnp_file_name, interface_file_name, input_file))
