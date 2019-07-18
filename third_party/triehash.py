#/usr/bin/env python3
#
# This is julian-klode's triehash translated to Python and modified for use with uxsdcxx.
#
# Copyright (C) 2016 Julian Andres Klode <jak@jak-linux.org>
# Copyright (C) 2019 Duck Deux <duck2@protonmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# TODO: Add the non-64bit-aligned trie alternative which is present in triehash.pl.

from typing import List, Tuple, Dict, Set, Union

# This implements a simple trie. Each node has three attributes:
#
# children - A hash of keys to other nodes
# value    - The value to be stored here
# label    - A named representation of the value.
#
# The key at each level of the trie can consist of one or more bytes, and the
# trie can be normalized to a form where all keys at a level have the same
# length using rebuild_tree().
class Trie:
	def __init__(self):
		self.children = {}
		self.value = None
		self.label = None

	# Return the largest power of 2 smaller or equal to the argument.
	# 16-bit reads are slow: don't include them
	def alignpower2(self, length):
		if length >= 8: return 8
		if length >= 4: return 4
		return 1

	# Split the key into a head block and a tail
	def split_key(self, key):
		split = self.alignpower2(len(key))
		return (key[:split], key[split:])

	# Given a key, a label, and a value, insert that into the tree, possibly
	# replacing an existing node.
	def insert(self, key, label, value):
		if len(key) == 0:
			self.label = label
			self.value = value
			return

		child, tail = self.split_key(key)
		if not self.children.get(child):
			self.children[child] = Trie()
		self.children[child].insert(tail, label, value)

	# Construct a new trie that only contains words of a given length. This
	# is used to split up the common trie after knowing all words, so we can
	# switch on the expected word length first, and have the per-trie function
	# implement simple longest prefix matching.
	def filter_depth(self, togo):
		new = Trie()
		if togo != 0:
			found = 0
			for key in sorted(self.children.keys()):
				if togo > len(key) or self.children[key].value is not None:
					child = self.children[key].filter_depth(togo - len(key))
					if child is not None:
						new.children[key] = child
						found = 1
			if not found: return None
		else:
			new.value = self.value
			new.label = self.label
		return new

	# (helper for rebuild_tree)
	# Reinsert all value nodes into the specified $trie, prepending $prefix
	# to their $paths.
	def reinsert_value_nodes_into(self, trie, prefix):
		if self.value is not None: trie.insert(prefix, self.label, self.value)
		for key in sorted(self.children.keys()):
			self.children[key].reinsert_value_nodes_into(trie, prefix+key)

	# This rebuilds the trie, choosing the smallest
	# split at each level, so that all keys at all levels have the same
	# length (so we can use a multi-byte switch).
	def rebuild_tree(self):
		new_split = 1e14
		for key in sorted(self.children.keys()):
			special_length = self.alignpower2(len(key))
			if special_length < new_split:
				new_split = special_length
		new = Trie()
		new.label = self.label
		new.value = self.value
		new.children = {}
		for key in sorted(self.children.keys()):
			head = key[:new_split]
			tail = key[new_split:]
			new.children[head] = Trie()
			self.children[key].reinsert_value_nodes_into(new.children[head], tail)
			new.children[head] = new.children[head].rebuild_tree()
		return new

def gen_prelude() -> str:
	out = "typedef uint16_t __attribute__((aligned(1))) triehash_uu16;\n"
	out += "typedef uint32_t __attribute__((aligned(1))) triehash_uu32;\n"
	out += "typedef uint64_t __attribute__((aligned(1))) triehash_uu64;\n"
	out += "static_assert(__alignof__(triehash_uu16) == 1);\n"
	out += "static_assert(__alignof__(triehash_uu32) == 1);\n"
	out += "static_assert(__alignof__(triehash_uu64) == 1);\n"
	out += "#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__\n"
	out += "#define onechar(c, s, l) (((uint64_t)(c)) << (s))\n"
	out += "#else\n"
	out += "#define onechar(c, s, l) (((uint64_t)(c)) << (l-8-s))\n"
	out += "#endif\n"
	return out

from pprint import pprint

def gen_lexer_body(alphabet: List[Tuple[str, str]]) -> str:
	out = ""
	trie = Trie()
	lengths = set()

	def case_label(key):
		x = len(key)
		return " | ".join(["onechar('%s', %d, %d)" % (key[i], 8*i, 8*x) for i in range(0, x)])

	def lexer_case(trie, indent="", index=0):
		out = ""
		if not trie.children:
			if trie.value: return indent + "return %s;\n"  % trie.value
			else: return indent + "break;\n"
		key_length = len(sorted(trie.children.keys())[-1])
		if key_length == 1:
			out += indent + "switch(in[%d]){\n" % index
		else:
			out += indent + "switch(*((triehash_uu%d*)&in[%d])){\n" % (8*key_length, index)
		for key in sorted(trie.children.keys()):
			out += indent + "case %s:\n" % case_label(key)
			out += lexer_case(trie.children[key], indent+"\t", index+len(key))
		out += indent + "}\n"
		return out

	for word, value in alphabet:
		trie.insert(word, word, value)
		lengths.add(len(word))
	out += "unsigned int len = strlen(in);\n"
	out += "switch(len){\n"
	for x in sorted(lengths):
		out += "case %d:\n" % x
		t = trie.filter_depth(x).rebuild_tree()
		out += lexer_case(t, "\t")
		out += "\tbreak;\n"
	out += "default:\n"
	out += "\tbreak;\n"
	out += "}\n"
	return out
