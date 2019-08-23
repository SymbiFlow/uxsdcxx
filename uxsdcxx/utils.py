import hashlib
import sys
import re

from .cpp_templates import cpp_keywords

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
