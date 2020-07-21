## uxsdcxx

[![Build Status](https://travis-ci.com/SymbiFlow/uxsdcxx.svg?branch=ci-and-cleanup)](https://travis-ci.com/SymbiFlow/uxsdcxx)

uxsdcxx is a tool which can generate [PugiXML](https://github.com/zeux/pugixml)-based C++ readers, validators and writers from XSD schemas. It generates an interface which can be implemented to bind the read/write logic to any kind of data structure.

It includes a script `uxsdcap`, which generates a Cap'n Proto schema and Cap'n Proto read/write logic from an XSD schema. The same interface is used to bind the data structure to Cap'n Proto logic.

Therefore, if all goes well, one gets a schema-generated validator/reader/writer which supports both XML and Cap'n Proto and works with existing data structures.

### XML support

Currently, uxsdcxx can generate code for a subset of XSD 1.0.

Notable warts are:

- [Data types](https://www.w3.org/TR/xmlschema-2/#built-in-datatypes):
	- Only `0` and `1` are read into `xs:boolean` types.
	- `xs:string` subtypes such as `ID` and `NMTOKEN` are not validated.
	- `xs:list`s are just read as strings.
	- Time types such as `xs:time` and `xs:date` are not supported.
	- `xs:integer` subtypes such as `xs:positiveInteger` are not validated.
- Simple type definitions:
	- `xs:list`s are read into strings.
	- `xs:union`s raise an error.
	- `xs:restriction`s are not enforced. An exception is `xs:enumeration` which maps to C++ enums.
- Elements:
	- `min_occurs` can only be 0 or 1. `max_occurs` can only be 1 or `unbounded`.
- [Identity constraint definitions](https://www.w3.org/TR/xmlschema-1/#cIdentity-constraint_Definitions), [notations](https://www.w3.org/TR/xmlschema-1/#cNotation_Declarations) and [wildcards](https://www.w3.org/TR/xmlschema-1/#Wildcards) are not supported.
- XML namespaces are not supported, since the underlying PugiXML does not support them.

### Installation

`pip install uxsdcxx`. Python 3.6 is required.
