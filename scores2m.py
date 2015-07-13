#!/usr/bin/env python
'''script to convert Jaakkola scores into Mercury/Prolog representation
'''
import fileinput
import re

vpat = re.compile(r'^([^-\s]+)\s+\d+\s*$')
spat = re.compile(r'^(-[\d.]+)\s+\d+\s+(.*)')

for line in fileinput.input():
    result = vpat.match(line)
    if result:
        thisvar = result.group(1)
        continue
    result = spat.match(line)
    if result:
        print 'score({2},[{1}],{0}).'.format(result.group(1),','.join(result.group(2).split()),thisvar)

    
