#!/usr/bin/env python3

import os
import sys

import xmlschema # type: ignore
from uxsdcxx.capnp import render_capnp_file
from uxsdcxx.schema import UxsdSchema

def main():
	input_file = os.path.abspath(sys.argv[1])
	base = os.path.splitext(os.path.basename(input_file))[0]
	capnp_file_name = base + "_uxsdcxx.capnp"
	cmdline = " ".join(sys.argv)
	schema = UxsdSchema(xmlschema.XMLSchema(input_file))
	capnp_file = open(capnp_file_name, "w")
	capnp_file.write(render_capnp_file(schema, cmdline, input_file))
	capnp_file.close()

if __name__ == "__main__":
	main()
