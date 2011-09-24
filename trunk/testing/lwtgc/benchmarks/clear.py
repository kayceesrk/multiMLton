from optparse import OptionParser
import sqlite3

def main():
	#Parse options
	parser = OptionParser()
	parser.add_option("-b", "--benchmark", dest="bmark", help="benchmarks")
	parser.add_option("-a", "--arguments", dest="arg", help="arguments")

	(options, args) = parser.parse_args()

	conn = sqlite3.connect ("/home/chandras/PLDI/resultsWB")
	c = conn.cursor ()

	if (options.bmark and (not options.arg)):
		b = options.bmark
		c.execute ("delete from heapRanges where benchmark=?", [b])
		c.execute ("delete from runTime where benchmark=?", [b])
		c.execute ("delete from completedRuns where benchmark=?", [b])
	elif (options.bmark and options.arg):
		b = options.bmark
		a = options.arg
		c.execute ("delete from heapRanges where benchmark=? and args=?", (b, a))
		c.execute ("delete from runTime where benchmark=? and args=?", (b,a))
		c.execute ("delete from completedRuns where benchmark=? and args=?", (b,a))
	else:
		sys.exit ("See usage")

	conn.commit ()


main ()
