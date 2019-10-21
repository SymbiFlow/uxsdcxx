#!/usr/bin/env python3

import os
import sys

import xmlschema # type: ignore
from uxsdcxx.cpp import render_header_file, render_impl_file
from uxsdcxx.schema import UxsdSchema

def main() -> None:
	input_file = os.path.abspath(sys.argv[1])
	base = os.path.splitext(os.path.basename(input_file))[0]
	header_file_name = base + "_uxsdcxx.h"
	impl_file_name = base + "_uxsdcxx.cpp"
	cmdline = " ".join(sys.argv)
	schema = UxsdSchema(xmlschema.validators.XMLSchema10(input_file))
	header_file = open(header_file_name, "w")
	header_file.write(render_header_file(schema, cmdline, input_file))
	header_file.close()
	impl_file= open(impl_file_name, "w")
	impl_file.write(render_impl_file(schema, cmdline, input_file, header_file_name))
	impl_file.close()

if __name__ == "__main__":
	main()
