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

	infile = open (dir + "/gc-summary.cumul.out", "r")
	for line in infile.readlines ():
		if line.startswith ("max heap size"):
			m = re.sub (r'max heap size: (.*?) bytes', r'\1', line).rstrip ()
			totalLocal = int (re.sub (r',', r'', m))

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

	outfile = open (dir + "/gc-summary.cumul.out", "w")
	outfile.write (output.decode("utf-8"))
	outfile.close ()

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
							"AllPairs": "floyd-warshall-amd64", \
							"Mandelbrot2": "mandelbrot-amd64", \
							"KClustering": "kclustering-amd64", \
							"TSP2": "tsp-amd64", \
							"Nucleic": "nucleic-amd64", \
							"CountGraphs": "count-graphs-amd64", \
							"GameOfLife": "lifeM-amd64", \
							"Mergesort": "mergesort-amd64", \
							"Raytrace": "raytrace-amd64"}
	args = {"BarnesHut2": "2048 512", \
					"AllPairs": "512 64", \
					"Mandelbrot2": "2048 128", \
					"KClustering": "0 50 700 70 0", \
					"TSP2": "", \
					"Nucleic": "512", \
					"CountGraphs": "1", \
					"GameOfLife": "64 300", \
					"Mergesort": "10000", \
					"Raytrace": "48"}
	numProcs = [16]
	return (progName, args, numProcs)

def testParameters():
	progName = {"CountGraphs": "count-graphs-amd64"}
	args = {"CountGraphs": "1"}
	numProcs = [16]
	return (progName, args, numProcs)

def main():
	reruns = 2

	#Parse options
	parser = OptionParser()
	parser.add_option("-d", "--database", dest="database", help="database location", \
										metavar="FILE", default="/home/chandras/PLDI/resultsUT")
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

	if (False and bool(input("Are you sure you want to drop the tables? "))):
		c.execute ("drop table if exists runTime")
		c.execute ("drop table if exists completedRuns")
		conn.commit ()

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
									and gckind=? and args=?", (b, n, "UT", args[b]))
			data = c.fetchone ()
			if (not data):
				print ("Benchmark: " + str(b) + " numProcs: " + str(n) + " -- heapRanges not found!!")
				continue

			maxHeapShared, maxHeapLocal = data
			maxHeapLocal = maxHeapLocal.replace('[', '').replace(']','').replace(',', ' ')
			maxHeapLocal  = shlex.split (maxHeapLocal)

			print (maxHeapShared)
			print (maxHeapLocal)

			for mlInt in maxHeapLocal:
				ml = bytesIntToString (mlInt, 1)
				ms = bytesIntToString (0, 1)
				atMLtons = ["number-processors " + str(n), \
										"fixed-heap " + str(ml), \
										"enable-timer 20000", \
										"gc-summary "]

				#run only if required
				shouldRun = True
				if (options.rerun == False):
					c.execute ("select * from completedRuns where benchmark=? and numProcs=? and maxHeapLocal=? \
											and maxHeapShared=? and gckind=? and args=?", (b, n, ml, ms, "UT", args[b]))
					data = c.fetchall ()
					if (data):
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
										(b, n, bytesIntToString (mlr, 1), bytesIntToString (msr, 1), bytesIntToString (m, 1), "UT", args[b], int(r)))
					c.execute ("insert into completedRuns values (?, ?, ?, ?, 'UT', ?)",\
										(b, n, ml, ms, args[b]))
					conn.commit ()

main ()
