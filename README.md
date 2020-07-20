## uxsdcxx

[![Build Status](https://travis-ci.com/SymbiFlow/uxsdcxx.svg?branch=ci-and-cleanup)](https://travis-ci.com/SymbiFlow/uxsdcxx)

### Disclaimer: Pre-1.0.0 software. Support for anything might break.

uxsdcxx is a tool to generate PugiXML-based C++ reader, validator and writer from an XSD schema. It can generate code for a subset of XSD 1.0.

It currently supports:

- Simple types with following exceptions:
	- `xs:list`s are just read into a string.
	- Only enumerations are supported as `xs:restriction`s of simple types.
	- Restricted string types such as `IDREF`, `NCName` etc. aren't validated.
- Complex types.
- Model groups(all, sequence and choice)
- Elements.
- Attributes except `xs:anyAttribute`.
	- Default values are supported.
	- When writing, non-zero default values are always written out.

It currently does not support:
- Anything that PugiXML can't read:
	- XML namespaces

### Getting started

`pip install uxsdcxx`. Use with `uxsdcxx.py foo.xsd`. Two files `foo_uxsdcxx.h` and `foo_uxsdcxx.cpp` will be created.

### API

All uxsdcxx functions live in a `namespace uxsd`.

##### 1. Root element class

For every root element in the schema, a class is generated with `load` and `write` functions. For instance,

```xml
<xs:element name="foo">
  <xs:complexType>
  ...
  </xs:complexType>
</xs:element>
```
results in this C++ code:
```c++
class foo : public t_foo {
public:
    pugi::xml_parse_result load(std::istream &is);
    void write(std::ostream &os);
};
```
`load()` loads from an input stream into this root element's structs and `write()` writes its content to a given output stream.

Note that root elements with simple types are not supported.

##### 2. Pools

uxsdcxx generates global pools to store multiply-occurring types.

```xml
<xs:complexType name="foo">
  <xs:sequence>
    <xs:element name="bar" type="bar" maxOccurs="unbounded"/>
  </xs:sequence>
</xs:complexType>
```
generates:
```c++
extern std::vector<t_bar> bar_pool;
[...]
struct foo {
    collapsed_vec<t_bar, bar_pool> bars;
};
```

A `collapsed_vec` is a size and an offset pointing into a pool. It provides contiguous memory while being able to store an unbounded number of elements. The main limitation of a `collapsed_vec` is that it's insertable only when its end points to the end of the pool.

Strings constitute a special case: a `char_pool` is generated for them to prevent many small allocations.

```xml
<xs:complexType name="foo">
  <xs:sequence>
    <xs:element name="bar" type="string" maxOccurs="unbounded"/>
  </xs:sequence>
</xs:complexType>
```
generates:
```c++
extern char_pool_impl char_pool;
struct foo {
    const char * bar;
};
```

The pools are freed by using utility functions `uxsd::clear_pools()` and `uxsd::clear_strings()`. `clear_strings` is provided separately since it can be useful to keep the strings around after freeing the generated structures.

##### 3. Data types

You can find the generated types for your schema in output header file `foo_uxsdcxx.h`. The mapping rules of XSD types to C++ types are such:

- `<xs:complexType>` definitions correspond to C++ structs `t_{name}`. For complexTypes in global scope, `name` refers to the `name` attribute of the type. For complexTypes defined inside elements, `name` refers to the `name` attribute of the parent element.
  - An `<xs:attribute>` generates a struct field with a C++ type corresponding to its `<xs:simpleType>` as defined below.
  - A model group such as `<xs:choice>`, `<xs:sequence>` or `<xs:all>` generates struct fields with C++ types corresponding to the types of the elements inside.
    - If an element can occur more than once, a `collapsed_vec<T, T_pool>` is generated.
    - If an element can occur zero times, another field `bool has_T` is generated to indicate whether the element is found.

- `<xs:simpleType>` can take many forms.
- `<xs:union>` corresponds to a tagged union type, such as:
```c++
struct union_foo {
    type_tag tag;
    union {
        double as_double;
        int as_int;
    };
};
```
- `<xs:list>` generates a `const char *`.
- Atomic builtins, such as `xs:string` or `xs:int` generate a field of the corresponding C++ type(`const char *`, `int`...)
- `<xs:restriction>`s of simple types are not supported, except one case where an `<xs:string>` is restricted to `<xs:enumeration>` values. C++ enums are generated for such constructs. As an example, the following XSD:
```xml
<xs:simpleType name="filler">
  <xs:restriction base="xs:string">
    <xs:enumeration value="FOO"/>
    <xs:enumeration value="BAR"/>
    <xs:enumeration value="BAZ"/>
  </xs:restriction>
</xs:simpleType>
```
generates a C++ enum:
```
enum class enum_filler {UXSD_INVALID = 0, FOO, BAR, BAZ};
```
