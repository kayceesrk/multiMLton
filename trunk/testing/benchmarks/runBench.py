import subprocess
import shlex
import re
import sqlite3
from optparse import OptionParser

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

  #For each benchmark plot the heap vs time graph
	for b in benchmarks:
		for n in [1, 2, 4, 8, 16]:
			c.execute ("select maxHeap, result from results where benchmark=? and numProcs=? and resultType=?", (b, n, "runTime"))
			for row in c.fetchall():
				print (row)
			print ("\n------------------------------------\n")


main ()
