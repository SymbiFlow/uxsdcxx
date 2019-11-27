#!/usr/bin/env python3

import os
import sys

import xmlschema # type: ignore
from uxsdcxx.capnp import render_capnp_file, render_header_file
from uxsdcxx.schema import UxsdSchema

def main():
	input_file = os.path.abspath(sys.argv[1])
	base = os.path.splitext(os.path.basename(input_file))[0]
	capnp_file_name = base + "_uxsdcxx.capnp"
	capnp_header_file_name = base + "_uxsdcxx_capnp_reader.h"
	cmdline = " ".join(sys.argv)
	schema = UxsdSchema(xmlschema.XMLSchema(input_file))
	with open(capnp_file_name, "w") as capnp_file:
		capnp_file.write(render_capnp_file(schema, cmdline, input_file))

	with open(capnp_header_file_name, "w") as header_file:
		header_file.write(render_header_file(schema, cmdline, capnp_file_name, base+"_uxsdcxx_interface.h", input_file))

if __name__ == "__main__":
	main()
