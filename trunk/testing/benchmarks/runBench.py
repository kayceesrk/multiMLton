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
	print ("\tCompleted in " + str(time) + " ms")
	return time

def hsizeToInt (s):
	intVal = int(s.replace('K','').replace('M','').replace('G',''))
	if s.endswith('K'):
		intVal *= 1024
	elif s.endswith('M'):
		intVal *= 1024*1024
	elif s.endswith('G'):
		intVal *= 1024*1024*1024
	return intVal


def main():
	#Parse options
	parser = OptionParser()
	parser.add_option("-d", "--database", dest="database", help="database location", \
										metavar="FILE", default="/home/chandras/PLDI/results")
	parser.add_option("-a", "--analyze-only", dest="analyzeOnly", default=False)

	(options, args) = parser.parse_args()

	#Connect to database
	conn = sqlite3.connect(str(options.database))
	c = conn.cursor ()

	#benchmark parameters
	benchmarks = ["BarnesHut"]
	progName = {"BarnesHut": "barnes-hutM-amd64"}
	numProcs = [1, 2, 4, 6, 8, 10, 12, 14, 16]
	maxHeap = {"BarnesHut": ["50M", "40M", "30M", "20M", "10M", "9M", "8M", "7M", "6M", "5M"]}

	c.execute('create table if not exists results \
						(benchmark text, numProcs int, maxHeap text, resultType text, result int)')

	if (not options.analyzeOnly):
		for b in benchmarks:
			for n in numProcs:
				for m in maxHeap[b]:
					atMLtons = ["number-processors " + str(n), \
											"max-heap " + m]
					r = run ("./" + str(b), str(progName[b]), atMLtons, "")
					c.execute ('insert into results values (?, ?, ?, ?, ?)', (b, n, m, "runTime", int(r)))
		conn.commit ()

	print ("Analyze")
	print ("-------")

	nodeKind = ['o-', 's--', 'D-.', 'x:', '^-', 'V--', '>-.', '<:']
	nodeIndex = 0

  #For each benchmark plot the heap vs time graph
	plt.xlabel ("Heap size relative to min heap size")
	plt.ylabel ("Time (ms)")
	plt.grid (True)
	for b in benchmarks:
		nodeIndex = 0
		plt.title (b + " -- Heap vs Time")
		log ("plotting heap vs time for " + b)
		for n in [1, 2, 4, 8, 16]:
			c.execute ("select maxHeap, result from results where benchmark=? and numProcs=? \
									and resultType=? and result!=0", (b, n, "runTime"))
			data = c.fetchall ()
			x = list (map (lambda v: hsizeToInt (v[0]), data))
			#Assumes that for a particular benchmark the smallest heap size is the same
			minX = min(x)
			x = [v/minX for v in x]
			y = list (map (lambda v: v[1], data))
			plt.plot (x, y, nodeKind[nodeIndex], label="Proc_"+str(n))
			nodeIndex += 1

		#plot the current graph
		plt.xlim(xmin = 0)
		plt.legend ()
		plt.savefig (b+"_heap_vs_time.eps")


main ()
