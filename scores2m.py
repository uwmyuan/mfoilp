#!/usr/bin/env python
'''script to convert Jaakkola scores into Mercury/Prolog representation
'''
import fileinput
import re

vpat = re.compile(r'^([^-\s]+)\s+\d+\s*$')
spat = re.compile(r'^(-[\d.]+)\s+\d+\s+(.*)')

fact_table = True
palim = 3
for line in fileinput.input():
    result = vpat.match(line)
    if result:
        thisvar = result.group(1)
        continue
    result = spat.match(line)
    if result:
        if fact_table:
            parents = result.group(2).split()
            while len(parents) < 3:
                parents.append('-1')
            print 'score_table({0},{1},{2},{3},{4}).'.format(
                thisvar,
                parents[0],
                parents[1],
                parents[2],
                result.group(1))
        else:
            print 'score({2},[{1}],{0}).'.format(result.group(1),','.join(result.group(2).split()),thisvar)

    
