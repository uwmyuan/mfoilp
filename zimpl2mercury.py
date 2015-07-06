#!/usr/bin/env python
'''Script to convert ZIMPL code to Mercury
'''
import fileinput
import re

setpattern = re.compile(r'\s*set\s+(?P<setname>[^\s:]+)\s*:=\s*\{(?P<elements>[^}]+)\};\s*')


def process_set(line):
   '''assume each set is defined as a set of strings
   will be translated to a discriminated union'''
   result = setpattern.match(line)
   typename = result.group('setname').lower()
   elts = [x.strip('" ').lower() for x in result.group('elements').split(',')]
   print ':- type {0} ---> {1};'.format(typename,';'.join(elts))
   predname = typename+'_gen'
   print ':- pred({0}::out) is multi.'.format(predname)
   for elt in elts:
      print '{0}({1}).'.format(predname,elt)
   
for line in fileinput.input():
   if line.startswith("set"):
      process_set(line)
