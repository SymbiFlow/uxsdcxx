import hashlib
import sys
import re

# See https://www.obj-sys.com/docs/xbv23/CCppUsersGuide/ch04.html.
atomic_builtins = {
	"string": "const char *",
	"boolean": "bool",
	"float": "float",
	"decimal": "int",
	"integer": "int",
	"nonPositiveInteger": "int",
	"negativeInteger": "int",
	"long": "long",
	"int": "int",
	"short": "short",
	"byte": "char",
	"nonNegativeInteger": "unsigned int",
	"unsignedLong": "unsigned long",
	"unsignedInt": "unsigned int",
	"unsignedShort": "unsigned short",
	"unsignedByte": "unsigned byte",
	"positiveInteger": "unsigned int",
	"double": "double",
}

atomic_builtin_load_formats = {
	"string": "char_pool.add(%s)",
	"boolean": "std::strtol(%s, NULL, 10)",
	"float": "std::strtof(%s, NULL)",
	"decimal": "std::strtol(%s, NULL, 10)",
	"integer": "std::strtol(%s, NULL, 10)",
	"nonPositiveInteger": "std::strtol(%s, NULL, 10)",
	"negativeInteger": "std::strtol(%s, NULL, 10)",
	"long": "std::strtoll(%s, NULL, 10)",
	"int": "std::strtol(%s, NULL, 10)",
	"short": "std::strtol(%s, NULL, 10)",
	"byte": "std::strtol(%s, NULL, 10)",
	"nonNegativeInteger": "std::strtoul(%s, NULL, 10)",
	"unsignedLong": "std::strtoull(%s, NULL, 10)",
	"unsignedInt": "std::strtoul(%s, NULL, 10)",
	"unsignedShort": "std::strtoul(%s, NULL, 10)",
	"unsignedByte": "std::strtoul(%s, NULL, 10)",
	"positiveInteger": "std::strtoul(%s, NULL, 10)",
	"double": "std::strtod(%s, NULL)",
}

cpp_keywords = ["alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept",
			"auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char8_t", "char16_t", "char32_t", "class",
			"compl", "concept", "const", "consteval", "constexpr", "const_cast", "continue", "co_await", "co_return",
			"co_yield", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit",
			"export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
			"namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
			"protected", "public", "reflexpr", "register", "reinterpret_cast", "requires", "return", "short", "signed",
			"sizeof", "static", "static_assert", "static_cast", "struct", "switch", "synchronized", "template", "this",
			"thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
			"virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"]

# https://stackoverflow.com/a/3431838
def md5(fname):
    hash_md5 = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()

def checked(x: str) -> str:
	"""Check against keywords, warn and rename if necessary."""
	if x in cpp_keywords:
		print("%s is a C++ keyword. Changing it to %s_." % (x, x), file=sys.stderr)
		return x + "_"
	return x

def to_token(x: str) -> str:
	return re.sub(r"[^a-zA-Z0-9_]", "_", x).upper()

def to_union_field_name(x: str) -> str:
	return "as_%s" % re.sub(r"[^a-zA-Z0-9_]", "_", x)

def to_comment_body(x: str) -> str:
	return "\n".join([" * " + line for line in x.split("\n") if line])

def indent(x: str, n: int=1) -> str:
	return "\n".join(["\t"*n + line if line else "" for line in x.split("\n")])

def pluralize(x: str) -> str:
	"""Rudimentary pluralization function. It's used to name lists of things."""
	if x[-2:] in ["sh", "ch", "ss"]: return x+"es"
	elif x[-1:] in ["s", "x", "z"]: return x+"es"
	else: return x+"s"
