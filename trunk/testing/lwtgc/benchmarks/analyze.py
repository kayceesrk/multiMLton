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

	infile = open (dir + "/gc-summary.cumul.out", "r")
	for line in infile.readlines ():
		if line.startswith ("max shared heap size"):
			m = re.sub (r'max shared heap size: (.*?) bytes', r'\1', line).rstrip ()
			shared = int (re.sub (r',', r'', m))

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
							"AllPairs": "floyd-warshall-amd64", \
							"Mandelbrot2": "mandelbrot-amd64", \
							"KClustering": "kclustering-amd64", \
							"TSP2": "tsp-amd64", \
							"Nucleic": "nucleic-amd64", \
							"MD5": "md5-amd64", \
							"CountGraphs": "count-graphs-amd64", \
							"GameOfLife": "lifeM-amd64", \
							"GameOfLife2": "life-amd64", \
							"Mergesort": "mergesort-amd64", \
							"Raytrace": "raytrace-amd64"}
	args = {"BarnesHut2": "2048 512", \
					"AllPairs": "1024 64", \
					"Mandelbrot2": "2048 128", \
					"KClustering": "0 50 700 70 0", \
					"TSP2": "", \
					"Nucleic": "512", \
					"MD5": "16", \
					"CountGraphs": "128 9", \
					"GameOfLife": "64 300", \
					"GameOfLife2": "64", \
					"Mergesort": "10000", \
					"Raytrace": "48"}
	numProcs = [16]
	return (progName, args, numProcs)

def testParameters():
	progName = {"CountGraphs": "count-graphs-amd64"}
	args = {"CountGraphs": "1"}
	numProcs = [16]
	return (progName, args, numProcs)

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
