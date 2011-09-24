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

def bytesIntToString (bytes, decimal):
	bytes = float(bytes)
	if bytes >= 1099511627776:
		terabytes = bytes / 1099511627776
		size = str (round (terabytes, decimal)) + "T"
	elif bytes >= 1073741824:
		gigabytes = bytes / 1073741824
		size = str (round (gigabytes, decimal)) + "G"
	elif bytes >= 1048576:
		megabytes = bytes / 1048576
		size = str (round (megabytes, decimal)) + "M"
	elif bytes >= 1024:
		kilobytes = bytes / 1024
		size = str (round (kilobytes, decimal)) + "K"
	else:
		size = str (round (bytes, decimal)) + "b"
	return size

def bytesIntToString2 (bytes, decimal):
	bytes = float(bytes)
	if bytes >= 1099511627776:
		terabytes = bytes / 1099511627776
		size = '%.1fT' % terabytes
		size = str (round (terabytes, decimal)) + "T"
	elif bytes >= 1073741824:
		gigabytes = bytes / 1073741824
		size = '%.1fG' % gigabytes
	elif bytes >= 1048576:
		megabytes = bytes / 1048576
		size = '%.1fM' % megabytes
	elif bytes >= 1024:
		kilobytes = bytes / 1024
		size = '%.1fK' % kilobytes
	else:
		size = '%.1fb' % bytes
	return size

def bytesStringToInt (s):
	intVal = float(s.replace('K','').replace('M','').replace('G',''))
	if s.endswith('K'):
		intVal *= 1024
	elif s.endswith('M'):
		intVal *= 1024*1024
	elif s.endswith('G'):
		intVal *= 1024*1024*1024
	return intVal

def log(s):
    if (logging):
        print (s)

def getMem(dir):
	totalLocal = 0
	shared = 0

	for file in os.listdir (dir):
		if fnmatch.fnmatch (file, 'gc-summary.*.out') and (not fnmatch.fnmatch (file, 'gc-summary.cumul.out')):
			infile = open (dir + "/" + file, "r")
			for line in infile.readlines ():
				if line.startswith ("max local heap size"):
					m = re.sub (r'max local heap size: (.*?) bytes', r'\1', line).rstrip ()
					totalLocal += int (re.sub (r',', r'', m))

	try:
		infile = open (dir + "/gc-summary.cumul.out", "r")
		for line in infile.readlines ():
			if line.startswith ("max shared heap size"):
				m = re.sub (r'max shared heap size: (.*?) bytes', r'\1', line).rstrip ()
				shared = int (re.sub (r',', r'', m))
	except:
		pass

	return totalLocal + shared, totalLocal, shared

def runAndGetOutput (args, dir):
	spArgs = shlex.split (args)
	proc = subprocess.Popen (spArgs, cwd=dir, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
	output = proc.communicate()[0]
	return output

def run (dir, prog, atMLtons, args):
	print ("\n-------------------------------\n")
	print ("DIR: " + dir + " PROG: " + prog)
	atMLtonString = ""
	for a in atMLtons:
		log ("\t" + a)
		atMLtonString += " " + a

	#run the program and capture the output
	args = "./" + str(prog) + " @MLton" + atMLtonString + " -- " + args
	log ("\t" + args)
	runAndGetOutput ("make clean-summaries", dir)
	output = runAndGetOutput (args, dir)

	#extract statistics from output
	output = re.sub (r'\n', r' ', str(output))
	if ("Out of memory" in output):
		time = "0"
		(m, ml, ms) = "0", "0", "0"
	else:
		time = re.sub (r'.*Time diff:\s*([0-9]*)\s*ms.*', r'\1', output)
		(m, ml, ms) = getMem (dir)
	try:
		int(time)
	except:
		time = "0"
		(m, ml, ms) = "0", "0", "0"
	print ("\tCompleted in " + str(time) + " ms")
	print ("\t\tTotal Mem: " + bytesIntToString (m, 1))
	print ("\t\tTotal Local Mem: " + bytesIntToString(ml, 1))
	print ("\t\tShared Mem: " + bytesIntToString (ms, 1))
	return (time, m, ml, ms)

def fullParameters():
	progName = {"BarnesHut2": "barnes-hut-amd64", \
							"KClustering": "kclustering-amd64", \
							"CountGraphs": "count-graphs-amd64", \
							"AllPairs": "floyd-warshall-amd64", \
							"Mandelbrot": "mandelbrot-amd64", \
							"TSP": "tsp-amd64", \
							"GameOfLife": "lifeM-amd64", \
							"Mergesort": "mergesort-amd64", \
							"Raytrace": "raytrace-amd64"}
	args = {"BarnesHut2": "1024 256", \
					"KClustering": "0 50 700 70 0", \
					"CountGraphs": "1", \
					"AllPairs": "512 64", \
					"Mandelbrot": "", \
					"TSP": "", \
					"GameOfLife": "64 300", \
					"Mergesort": "10000", \
					"Raytrace": "64"}
	numProcs = [16]
	return (progName, args, numProcs)

def testParameters():
	progName = {"CountGraphs": "count-graphs-amd64"}
	args = {"CountGraphs": "1"}
	numProcs = [16]
	return (progName, args, numProcs)


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
	numPartitions = 3
	pointsPerPartition = 4

	if (min > max):
		return []

	return getPointsRec (min, max, numPartitions, pointsPerPartition)

def maxHeapLocalValues (b, n, progName, args, c):
	atMLtons = ["number-processors " + str(n), \
							"enable-timer 20000", \
							"gc-summary individual"]
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
	points2 = getPointsInRange (points[0], points[1], 10)
	points += points2
	points.sort ()
	print ("values for maxHeapLocal: " + str ([bytesIntToString (x, 1) for x in points]))
	return points

def maxHeapSharedValues (b, n, progName, args, c):
	atMLtons = ["number-processors " + str(n), \
							"enable-timer 20000", \
							"gc-summary individual"]
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
	points2 = getPointsInRange (points[0], points[1], 10)
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
