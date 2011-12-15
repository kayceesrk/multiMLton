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

def avgSameXes (x,y):
	xRes = []
	yRes = []
	count = 1
	yCur = -1
	for i in range (1, len(x)+1):
		if (i < len(x) and x[i] == x[i-1]):
			yCur += y[i]
			count += 1
		else:
			xRes.append (x[i-1])
			yRes.append (yCur/count)
			if (i < len(x)):
				yCur = y[i]
				count = 1

	return (xRes, yRes)

def main():
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
		benchmarks = ["BarnesHut2", "CountGraphs", "KClustering"]
	(progName, args, numProcs) = fullParameters ()

	nodeKind = ['o-', 's--', 'D-.', 'x:', '^-', 'V--', '>-.', '<:']

	for b in benchmarks:
		#intialize
		nodeIndex = 0
		shouldPlot = False

		#For each benchmark plot the heap vs time graph
		plt.xlabel ("Heap size relative to min heap size")
		plt.ylabel ("Time (ms)")
		plt.grid (True)
		plt.title (b + " -- Heap vs Time")
		log ("preparing data for plotting heap vs time for " + b)

		#calculate the minimum x
		c.execute ("select maxHeap from runTime where benchmark=? and gckind='WB' and result!=0 and args=?", (b, args[b]))
		data = list (map (lambda v: bytesStringToInt (v[0]), c.fetchall ()))
		if data:
			minX = min(data)
		else:
			minX = 0

		for n in numProcs:
			c.execute ("select maxHeap, result from runTime where benchmark=? and numProcs=? \
									and result!=0 and gckind='WB' and args=?", (b, n, args[b]))
			data = c.fetchall ()
			x = list (map (lambda v: bytesStringToInt (v[0]), data))
			if x: #x is not empty
				shouldPlot = True
				y = list (map (lambda v: v[1], data))

				#debug
				z = list (zip (x,y))
				z.sort ()
				for (xi,yi) in z:
					print (str(xi) + " " + str(yi))

				x = [v/minX for v in x]
				z = list (zip (x,y))
				z.sort ()
				x,y = list(zip (*z))
				l = "P="+str(n)
				#plt.plot (x, y, nodeKind[nodeIndex], label=l)
				#nodeIndex += 1
				#nodeIndex %= len(nodeKind)

				xRes, yRes = avgSameXes (x, y)
				plt.plot (xRes, yRes, nodeKind [nodeIndex], label=l)
				nodeIndex += 1
				nodeIndex %= len(nodeKind)

			if shouldPlot:
				log ("plotting heap vs time for " + b)
				#plot the current graph
				plt.xlim(xmin = 0)
				plt.legend ()
				plt.savefig (b+"_WB_local_heap_vs_time.eps")
				plt.close ()


main ()
