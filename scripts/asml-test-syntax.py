#!/usr/bin/env python
import sys
import re
ASML_PATTERN = re.compile(r'(\blet\s+_(\S).*\s)*(?P<main>\blet\s+_\s*=\s*\S.*)', re.I + re.S)
FUNC_PATTERN = re.compile(r'(?P<fundef>\blet\s+_\S+(.*?)(?=\s+let\s+_))', re.I + re.S)

def test(prog):
    prog_trimmed = prog.strip()
    return ASML_PATTERN.match(prog_trimmed)

def func(prog):
    prog_trimmed = prog.strip()
    return FUNC_PATTERN.finditer(prog_trimmed);
    

if len(sys.argv) < 2:
    print ("Usage: {0} <asml_file>".format(sys.argv[0]))
    sys.exit(1)

fname = sys.argv[1]
with open(fname, 'r') as f:
    prog = f.read()
    matched = test(prog)
    if not matched:
        sys.stderr.write("{0} is not a valid asml file\n".format(fname))
        sys.exit(1)

    print "Functions:"
    fm = func(prog)
    for f in fm:
        print f.group('fundef')
        print '---'
    print "Main:"
    print matched.group('main')
