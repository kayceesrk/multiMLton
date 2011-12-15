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

logging=True

def main():
	reruns = 1

	#Parse options
	parser = OptionParser()
	parser.add_option("-d", "--database", dest="database", help="database location", \
										metavar="FILE", default="/home/chandras/PLDI/resultsWB")
	parser.add_option("-b", "--benchmark", dest="bmarkList", action="append", \
										help="run only the given benchmarks")
	parser.add_option("-r", "--rerun", dest="rerun", default=False, \
										help="Rerun a specific test even if the corresponding result is available in the database")

	(options, args) = parser.parse_args()

	#Connect to database
	conn = sqlite3.connect(str(options.database))
	c = conn.cursor ()

	#benchmark parameters
	if (options.bmarkList):
		benchmarks = options.bmarkList
	else:
		benchmarks = ["BarnesHut2",	"CountGraphs", "KClustering"]
	(progName, args, numProcs) = fullParameters ()

	#create the completed run if it is not already present
	c.execute('create table if not exists completedRuns \
						(benchmark text, numProcs int, maxHeapLocal text, maxHeapShared text, gckind text, args text)')

	#create the runtime table if it is not already present
	c.execute('create table if not exists runTime \
						(benchmark text, numProcs int, maxHeapLocal text, \
						 maxHeapShared text, maxHeap text, gckind text, args text, result int)')

	for b in benchmarks:
		for n in numProcs:
			c.execute ("select maxSHV, maxLHV from heapRanges where benchmark=? and numProcs=? \
									and gckind=? and args=?", (b, n, "WB", args[b]))
			data = c.fetchone ()
			if (not data):
				print ("Benchmark: " + str(b) + " numProcs: " + str(n) + " -- heapRanges not found!!")
				continue

			maxHeapShared, maxHeapLocal = data
			maxHeapShared = maxHeapShared.replace('[', '').replace(']','').replace(',',' ')
			maxHeapShared  = shlex.split (maxHeapShared)
			maxHeapLocal = maxHeapLocal.replace('[', '').replace(']','').replace(',', ' ')
			maxHeapLocal  = shlex.split (maxHeapLocal)

			totalRuns = len(maxHeapShared)
			runCount = 0
			for i in range (0, totalRuns):
				mlInt = maxHeapLocal[i]
				msInt = maxHeapShared[i]
				runCount += 1
				print ("\n-------------------------------\n")
				print ("In Benchmark: " + str(b) + " numProcs: " + str(n))
				print ("In run " + str(runCount) + " of " + str(totalRuns))

				ml = bytesIntToString (mlInt, 1)
				ms = bytesIntToString (msInt, 1)
				atMLtons = ["number-processors " + str(n), \
										"max-heap-local " + str(ml), \
										"max-heap-shared " + str(ms), \
										"gc-summary individual"]
										#"enable-timer 20000", \

				#run only if required
				shouldRun = True
				if (options.rerun == False):
					c.execute ("select * from completedRuns where benchmark=? and numProcs=? and maxHeapLocal=? \
											and maxHeapShared=? and gckind=? and args=?", (b, n, ml, ms, "WB", args[b]))
					data = c.fetchall ()
					if (data):
						print ("skipping...")
						shouldRun = False

				failed = 0
				if (shouldRun):
					r, m, mlr, msr = 0, 0, 0, 0
					for i in range(0, reruns):
						(_r, _m, _mlr, _msr) = run ("./" + str(b), str(progName[b]), atMLtons, args[b])
						if int(_r) == 0:
							failed += 1
							print ("Failed: " + str(failed))
						r += int(_r)
						m += int(_m)
						mlr += int(_mlr)
						msr += int(_msr)

					if (reruns-failed) != 0:
						r, m, mlr, msr = r/(reruns-failed), m/(reruns-failed), mlr/(reruns-failed), msr/(reruns-failed)
					else:
						r, m, mlr, msr = 0, 0, 0, 0
					c.execute ('insert into runTime values (?, ?, ?, ?, ?, ?, ?, ?)', \
										(b, n, bytesIntToString (mlr, 1), bytesIntToString (msr, 1), bytesIntToString (m, 1), "WB", args[b], int(r)))
					c.execute ("insert into completedRuns values (?, ?, ?, ?, 'WB', ?)",\
										(b, n, ml, ms, args[b]))
					conn.commit ()

main ()
