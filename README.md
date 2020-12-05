## uxsdcxx

[![Build Status](https://travis-ci.com/SymbiFlow/uxsdcxx.svg?branch=ci-and-cleanup)](https://travis-ci.com/SymbiFlow/uxsdcxx)
[![Coverage](coverage.svg)](coverage.svg)

uxsdcxx is a tool which can generate [PugiXML](https://github.com/zeux/pugixml)-based C++ serializers from XSD schemas. It also generates an interface which should be implemented to bind the serialization logic to any kind of data structure.

An `uxsdcap` script is included which generates a Cap'n Proto schema and Cap'n Proto serialization logic from an XSD schema. The same interface is used to bind the data structure to the Cap'n Proto serializer.

Therefore, if all goes well, one gets a schema-generated serializer which supports both XML and Cap'n Proto and works with existing data structures.

### XSD support

Currently, uxsdcxx can generate code for a subset of XSD 1.0.

Notable quirks are:

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

If installing for uxsdcxx development, run:

```
$ git clone --recursive https://github.com/SymbiFlow/uxsdcxx
$ # In a virtualenv:
$ pip install -r requirements.txt
$ pip install -e .
```

### Quickstart

Let's generate C++ serialization code for the `hello.xsd` schema with uxsdcxx.

```xsd
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <xsd:complexType name="hello">
    <xsd:sequence>
      <xsd:element name="greeting" type="xsd:string"/>
      <xsd:element name="name" type="xsd:string" maxOccurs="unbounded"/>
    </xsd:sequence>
  </xsd:complexType>
  <xsd:element name="hello" type="hello"/>
</xsd:schema>
```

An XML file conforming to this schema would look like:

```xml
<hello>
  <greeting>Hello</greeting>
  <name>sun</name>
  <name>moon</name>
  <name>world</name>
</hello>
```

We first generate the C++ code by running `uxsdcxx hello.xsd`. After here, the Python API ends and the C++ API starts.

This command will generate two files named `hello_uxsdcxx.h` and `hello_uxsdcxx_interface.h`. `hello_uxsdcxx.h` contains top level read/write functions and internals, while `hello_uxsdcxx_interface.h` contains the interface you will need to implement. All uxsd related functions and classes live inside an `uxsd` namespace.

```cpp
template<typename ContextTypes=DefaultHelloContextTypes>
class HelloBase {
public:
	virtual ~HelloBase() {}
	virtual void start_load(const std::function<void(const char*)> *report_error) = 0;
	virtual void finish_load() = 0;
	virtual void start_write() = 0;
	virtual void finish_write() = 0;
	virtual void error_encountered(const char * file, int line, const char *message) = 0;
	...
```

We can copy this class into a `HelloSerializer` class inheriting `uxsd::HelloBase`, set up some data structures and start implementing the functions to bind the serialization logic to the data structures.

```cpp
/* HelloSerializer.h */
#include <string>
#include <vector>
#include "hello_uxsdcxx_interface.h"

class HelloSerializer: public uxsd::HelloBase<uxsd::DefaultHelloContextTypes> {
public:
	std::string greeting;
	std::vector<std::string> names;

	HelloSerializer(){}
	void start_load(const std::function<void(const char*)> *report_error){}
	void finish_load(){}
	void start_write(){}
	void finish_write(){}
	void error_encountered(const char * file, int line, const char *message){
		std::cerr << file << ":" << line << ": " << message << std::endl;
		throw std::runtime_error("Error while reading file.");
	}
	...
```

Here, we set up a `std::string` to hold the greeting and a vector of strings to hold the names in the file. These will hold our data.

The `start` and `finish` functions are called at the very start or end of operations. They can be left empty for `hello` purposes.

`error_encountered` is called when a validation error occurs and it is expected to throw an exception to unwind the stack.

```cpp
	...
	inline void set_hello_greeting(const char * greeting, void *& /*ctx*/){
		this->greeting = greeting;
	}
	inline const char * get_hello_greeting(void *& /*ctx*/){
		return this->greeting.c_str();
	}
	inline void preallocate_hello_name(void *& /*ctx*/, size_t size){}
	inline void add_hello_name(const char * name, void *& /*ctx*/){
		this->names.push_back(name);
	}
	inline size_t num_hello_name(void *& /*ctx*/){
		return this->names.size();
	}
	inline const char * get_hello_name(size_t n, void *& /*ctx*/){
		return this->names[n].c_str();
	}
}
```

These are the data access functions. The `set_`, `preallocate_` and `add_` functions are called when reading a file, whereas the `get_` and `num_` functions are called when writing to a file. For types in a sequence, a `n` parameter is provided to index the sequence.

We have implemented the data access functions to point to our internal structs. This closes the `HelloSerializer` class. Now, we can write a main program to read some XML.

```cpp
/* main.cpp */
#include <fstream>
#include <iostream>

#include "hello_uxsdcxx.h"
#include "HelloSerializer.h"

int main(int argc, char **argv){
	HelloSerializer hello;
	void *context;
	std::ifstream f(argv[1]);
	try {
		uxsd::load_hello_xml(hello, context, argv[1], f);
	} catch (std::runtime_error &e) {
		std::cout << e.what() << std::endl;
		return 1;
	}

	/* $greet $things. */
	std::cout << hello.greeting;
	for(auto& name : hello.names){
		std::cout << " " << name;
	}
	std::cout << "!" << std::endl;
}
```

Note that `hello_uxsdcxx.h` depends on PugiXML, so we have to link PugiXML when compiling.

```bash
$ g++ --std=c++11 -o hello main.cpp pugixml/src/pugixml.cpp -Ipugixml/src/
$ ./hello hello.xml
Hello sun moon world!
```

What if our `hello.xml` is malformed? For instance, the schema suggests that `<greeting>` should appear only once and before `<name>`s. Let's write a rebel file:

```xml
<!-- hello_rebel.xml -->
<hello>
  <greeting>Hello</greeting>
  <name>sun</name>
  <name>moon</name>
  <greeting>What's up</greeting>
  <name>world</name>
</hello>
```

This should cause an error:

```bash
$ ./hello hello_rebel.xml
hello_rebel.xml:5: Expected name, found greeting
Error while reading file.
```

This concludes our Hello World example. See the [docs][docs] for more examples and API reference.

### License

Apache 2.0. See [LICENSE](LICENSE).

[docs]: https://uxsdcxx.readthedocs.io/en/ci-and-cleanup/
