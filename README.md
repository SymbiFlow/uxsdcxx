## uxsdcxx

This is a tool to generate C++ mappings from XSD schemas.

**Warning: This is a work in progress! It doesn't do anything this README says.**

### API

Generates a namespace with the XSD file's name. In the namespace are:

* Struct definitions mapping to complex types.
* Global variables containing the root-level elements.
* A `read(const char *filename)` function.

When `xml_file::read()` is called, the file is validated and loaded into the generated structures.
