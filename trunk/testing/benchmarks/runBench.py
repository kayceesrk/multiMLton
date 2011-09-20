import subprocess
import shlex
import re
import sqlite3
from optparse import OptionParser
import numpy
import matplotlib.pyplot as plt

logging=True

def log(s):
    if (logging):
        print (s)

def run (dir, prog, atMLtons, args):
	print ("\n-------------------------------\n")
	print ("DIR: " + dir + " PROG: " + prog)
	atMLtonString = ""
	for a in atMLtons:
		log ("\t" + a)
		atMLtonString += " " + a

	#run the program and capture the output
	args = "./" + str(prog) + " @MLton" + atMLtonString + " gc-summary -- " \
			+ args

	log ("\t" + args)

	spArgs = shlex.split (args)
	proc = subprocess.Popen (spArgs, cwd=dir, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
	output = proc.communicate()[0]

	#extract statistics from output
	output = re.sub (r'\n', r' ', str(output))
	if ("Out of memory" in output):
		time = "0"
	else:
		time = re.sub (r'.*Time diff:\s*([0-9]*)\s*ms.*', r'\1', output)
	try:
		int(time)
	except:
		time = 0
	print ("\tCompleted in " + str(time) + " ms")
	return time

def hsizeToInt (s):
	intVal = float(s.replace('K','').replace('M','').replace('G',''))
	if s.endswith('K'):
		intVal *= 1024
	elif s.endswith('M'):
		intVal *= 1024*1024
	elif s.endswith('G'):
		intVal *= 1024*1024*1024
	return intVal


def fullParameters():
	progName = {"BarnesHut": "barnes-hutM-amd64", \
							"BarnesHut2": "barnes-hut-amd64", \
							"AllPairs": "floyd-warshall-amd64", \
							"Mandelbrot": "mandelbrot-amd64", \
							"KClustering": "kclustering-amd64", \
							"TSP": "tsp-amd64", \
							"CountGraphs": "count-graphs-amd64", \
							"GameOfLife": "lifeM-amd64", \
							"Mergesort": "mergesort-amd64", \
							"Raytrace": "raytrace-amd64"}
	args = {"BarnesHut": "", \
					"BarnesHut2": "4096 64", \
					"AllPairs": "512 64", \
					"Mandelbrot": "", \
					"KClustering": "0 256 200 50 0", \
					"TSP": "", \
					"CountGraphs": "", \
					"GameOfLife": "64 300", \
					"Mergesort": "10000", \
					"Raytrace": "64"}
	numProcs = [1, 2, 4, 8, 16]
	maxHeap = {"BarnesHut": ["50M", "40M", "30M", "20M", "10M", "9M", "8M", "7M", \
													 "6.5M", "6M", "5.5M"], \
					   "BarnesHut2": ["50M", "40M", "30M", "20M", "10M", "9M", "8M", "7M", \
													 "6.5M", "6M", "5.5M"], \
						 "AllPairs": ["50M", "40M", "30M", "20M", "10M", "9M", "8M", "7M", \
						 							"6.5M", "6M", "5.5M"],
						 "Mandelbrot": ["50M", "40M", "30M", "25M", "20M", "18M", "16M", \
								 						"14M", "12M", "11M", "10M", "9M", "8M", "7M", "6M", \
														"5M", "4M", "3M"], \
						 "KClustering": ["20M", "15M", "12M", "11M", "10M", "9M", "8M", "7.5M", \
							 							 "7M", "6.5M", "6M"], \
					   "TSP": ["200M", "150M", "100M", "96M", "95M", "94.5M"], \
						 "CountGraphs": ["25M", "15M", "10M", "6M", "4M", "3M", "2.5M", "2M", "1M"], \
						 "GameOfLife": ["25M", "15M", "10M", "6M", "4M", "3M", "2.5M", "2M", "1M"], \
						 "Mergesort": ["100M", "50M", "40M", "30M", "25M", "22M", "20M", "19M", "18M"], \
						 "Raytrace": ["30M", "25M", "22M", "20M", "18M", "16M", "15M"]}
	return (progName, args, numProcs, maxHeap)

def testParameters():
	progName = {"BarnesHut": "barnes-hutM-amd64", \
							"AllPairs": "floyd-warshall-amd64", \
							"Mandelbrot": "mandelbrot-amd64"}
	args = {"BarnesHut": "", \
					"AllPairs": "512 64", \
					"Mandelbrot": ""}
	numProcs = [1, 2, 4, 8, 16]
	maxHeap = {"BarnesHut": ["50M"], \
						 "AllPairs": ["50M", "40M"], \
						 "Mandelbrot": ["50M", "40M"]}
	return (progName, args, numProcs, maxHeap)


def main():
	#Parse options
	parser = OptionParser()
	parser.add_option("-d", "--database", dest="database", help="database location", \
										metavar="FILE", default="/home/chandras/PLDI/results")
	parser.add_option("-a", "--analyze-only", dest="analyzeOnly", default=False, \
										help="Skip running the benchmarks and only run the analysis")
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
		benchmarks = ["BarnesHut", "AllPairs", "Mandelbrot", "KClustering", "TSP", \
									"CountGraphs", "GameOfLife", "Mergesort", "Raytrace", "BarnesHut2"]
	(progName, args, numProcs, maxHeap) = fullParameters ()

	#create the runtime table if it is not already present
	c.execute('create table if not exists runTime \
						(benchmark text, numProcs int, maxHeap text, result int)')

	if (not options.analyzeOnly):
		for b in benchmarks:
			for n in numProcs:
				for m in maxHeap[b]:
					atMLtons = ["number-processors " + str(n), \
											"max-heap " + m]

					#run only if required
					shouldRun = True
					if (options.rerun == False):
						c.execute ("select result from runTime where benchmark=? and numProcs=? and maxHeap=?", \
											(b, n, m))
						data = c.fetchall ()
						if (data and int(data[0][0]) != 0):
							shouldRun = False

					if (shouldRun):
						r = run ("./" + str(b), str(progName[b]), atMLtons, args[b])
						c.execute ('delete from runTime where benchmark=? and numProcs=? and maxHeap=?', \
											(b, n, m))
						c.execute ('insert into runTime values (?, ?, ?, ?)', (b, n, m, int(r)))
						conn.commit ()

	print ("Analyze")
	print ("-------")

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
		c.execute ("select distinct maxHeap from runTime where benchmark=? and result!=0", [b])
		data = list (map (lambda v: hsizeToInt (v[0]), c.fetchall ()))
		if data:
			minX = min(data)
		else:
			minX = 0

		for n in [1, 2, 4, 8, 16]:
			c.execute ("select maxHeap, result from runTime where benchmark=? and numProcs=? \
									and result!=0", (b, n))
			data = c.fetchall ()
			x = list (map (lambda v: hsizeToInt (v[0]), data))
			if x: #x is not empty
				shouldPlot = True
				x = [v/minX for v in x]
				y = list (map (lambda v: v[1], data))
				plt.plot (x, y, nodeKind[nodeIndex], label="Proc="+str(n))
				nodeIndex += 1

		if shouldPlot:
			log ("plotting heap vs time for " + b)
			#plot the current graph
			plt.xlim(xmin = 0)
			plt.legend ()
			plt.savefig (b+"_heap_vs_time.eps")
			plt.close ()


main ()
