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

def getPointsInRange (min, max, numPoints):
	result = []
	step = (max - min)/(numPoints)
	for i in range (0, numPoints):
		result.append (round (min + i*step))
	return result

def getPointsRec (min, max, numPartitions, pointsPerPartition):
	res = []
	if (numPartitions):
		cur = (min + max)/2
		if (numPartitions == 1):
			res += getPointsInRange (min, max, pointsPerPartition)
		else:
			res += getPointsInRange (cur, max, pointsPerPartition)
		res += getPointsRec (min, round (cur), numPartitions - 1, pointsPerPartition)
	return res

def getPoints (min, max):
	#Total number of points = numPartitions * pointsPerPartition
	numPartitions = 4
	pointsPerPartition = 4

	if (min > max):
		return []

	return getPointsRec (min, max, numPartitions, pointsPerPartition)

def maxHeapLocalValues (b, n, progName, args, c):
	atMLtons = ["number-processors " + str(n), \
							"gc-summary individual"]
							#"enable-timer 20000", \
	(t, m, ml, ms) = run ("./" + str(b), str(progName[b]), atMLtons, args[b])
	maxHeapLocalMax = ml
	if int(t) == 0:
		sys.exit ("Initial run failed. Exiting....")

	min = 0
	max = maxHeapLocalMax
	while (bytesIntToString (min, 0) != bytesIntToString (max, 0)):
		print (bytesIntToString (min, 0))
		print (bytesIntToString (max, 0))
		cur = (min + max)/2
		newAtMLtons = atMLtons + ["max-heap-local " + bytesIntToString (round(cur), 1)]
		(t, m, ml, ms) = run ("./" + str(b), str(progName[b]), newAtMLtons, args[b])
		if int(t) == 0:
			print ("Repeat")
			(t, m, ml, ms) = run ("./" + str(b), str(progName[b]), newAtMLtons, args[b])
			if int(t) == 0:
				min = cur
			else:
				max = cur
		else:
			max = cur
	maxHeapLocalMin = cur

	points = getPoints (maxHeapLocalMin, maxHeapLocalMax)
	points.sort ()
	points2 = getPointsInRange (points[0], points[1], 15)
	points += points2
	points.sort ()
	print ("values for maxHeapLocal: " + str ([bytesIntToString (x, 1) for x in points]))
	return points

def maxHeapSharedValues (b, n, progName, args, c):
	atMLtons = ["number-processors " + str(n), \
							"gc-summary individual"]
							#"enable-timer 20000", \
	(t, m, ml, ms) = run ("./" + str(b), str(progName[b]), atMLtons, args[b])
	maxHeapSharedMax = ms
	if int(t) == 0:
		sys.exit ("Initial run failed. Exiting....")

	min = 0
	max = maxHeapSharedMax
	while (bytesIntToString (min, 0) != bytesIntToString (max, 0)):
		print (bytesIntToString (min, 0))
		print (bytesIntToString (max, 0))
		cur = (min + max)/2
		newAtMLtons = atMLtons + ["max-heap-shared " + bytesIntToString (round(cur), 1)]
		(t, m, ml, ms) = run ("./" + str(b), str(progName[b]), newAtMLtons, args[b])
		if int(t) == 0:
			min = cur
		else:
			max = cur
	maxHeapSharedMin = cur

	points = getPoints (maxHeapSharedMin, maxHeapSharedMax)
	points.sort ()
	points2 = getPointsInRange (points[0], points[1], 5)
	points += points2
	points.sort ()
	print ("values for maxHeapShared: " + str ([bytesIntToString (x, 1) for x in points]))

	return points

def main():

	#Parse options
	parser = OptionParser()
	parser.add_option("-b", "--benchmark", dest="bmarkList", action="append", \
										help="run only the given benchmarks")
	(options, args) = parser.parse_args()

	#benchmark parameters
	if (options.bmarkList):
		benchmarks = options.bmarkList
	else:
		benchmarks = ["BarnesHut2",	"CountGraphs", "KClustering"]

	#Connect to database
	conn = sqlite3.connect("/home/chandras/PLDI/resultsWB")
	c = conn.cursor ()

	c.execute ("create table if not exists heapRanges \
							(benchmark text, numProcs int, gckind text, args text, maxSHV text, maxLHV text)")

	(progName, args, numProcs) = fullParameters ()

	for b in benchmarks:
		for n in numProcs:
			shouldRun = False
			c.execute ("select * from heapRanges where benchmark=? and numProcs=? and args=? and gckind=?",
								 (b, n, args[b], "WB"))

			if (not c.fetchall ()):
				shouldRun = True

			if (shouldRun):
				c.execute ("delete from heapRanges where benchmark=? and numProcs=? and args=? and gckind=?", \
									 (b, n, args[b], "WB"))
				maxSHV = maxHeapSharedValues (b, n, progName, args, c)
				maxLHV = maxHeapLocalValues (b, n, progName, args, c)
				#insert into DB
				c.execute ("insert into heapRanges values (?, ?, ?, ?, ?, ?)",
										(b, n, "WB", args[b], str(maxSHV), str(maxLHV)))
				conn.commit ()

main ()
