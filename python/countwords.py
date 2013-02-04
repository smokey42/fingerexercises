#!/usr/bin/env python
# encoding: utf-8

from collections import Counter

def words(fileobj):
    for line in fileobj.readlines():
        for word in line.split():
            yield word

with open("input.txt", "r") as text:
    for entry in Counter(words(text)).most_common():
        print "%s, %d" % entry
