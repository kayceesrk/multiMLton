import subprocess
import shlex
import re
from optparse import OptionParser
import numpy
import matplotlib.pyplot as plt
import sys
import os
import fnmatch
import locale
import sqlite3
import common

def main ():
	#Parse options
	parser = OptionParser()

	parser.add_option("-b", "--benchmark", dest="bmarkList", action="append", \
										help="run only the given benchmarks")
	parser.add_option("-a", "--argument", dest="arguments", help="make arguments")

	(options, args) = parser.parse_args()

	(progName, args, numProcs) = common.fullParameters ()
	if (options.bmarkList):
		benchmarks = options.bmarkList
	else:
		benchmarks = progName.keys()

	procs = []
	for b in benchmarks:
		args = "make -B " + str(options.arguments) + " " + str(progName[b])
		print (args)
		spArgs = shlex.split (args)
		procs.append (subprocess.Popen (spArgs, cwd=b, stderr=subprocess.STDOUT, stdout=subprocess.PIPE))

	for p in procs:
		output = p.communicate()[0]

main ()
