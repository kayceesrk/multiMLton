import subprocess
import shlex
import re
import signal
import texttable as tt
from optparse import OptionParser

logging=True

def log(s):
	if (logging):
		print s

class Alarm(Exception):
	pass

def alarm_handler(signum, frame):
	raise Alarm

def wipeProcess(prog):
	#wipeProcess may get stuck. Hence, install signal handler
		signal.signal(signal.SIGALRM, alarm_handler)
		signal.alarm(5)
		try:
			log ("\tstarting wipeProcess: " + str(prog))
			args = "./wipe " + str(prog)
			log ("\t" + args)
			args = shlex.split (args)
			proc = subprocess.Popen (args, cwd="/shared/chandras", \
					stdout=subprocess.PIPE)
			output = proc.stdout.read ()
			log ("\tfinished wipeProcess")
			signal.alarm(0)
		except Alarm:
			#Try again if timeout. XXX may go into infinite loop
			log ("\tTimeout!!")
			proc.kill ()
			wipeProcess(prog)

def run(directory, prog, numProcs, pargs, timeout):
	wipeProcess(prog)

	#run the program and capture the output
	args = "./rccerun -nue "+ str(numProcs) +" -f rc.hosts " + str(directory) + "/" + str(prog) + " " + str(pargs)
	log ("\t" + args)
	args = shlex.split (args)
	signal.signal(signal.SIGALRM, alarm_handler)
	signal.alarm(int(timeout))
	print ("\tSetting timeout to " + str (timeout))
	try:
		proc = subprocess.Popen (args, cwd="/shared/chandras", stderr = subprocess.STDOUT)
		proc.communicate()
		signal.alarm(0)
	except Alarm:
		print "\tTimeout!!"
		proc.kill ()
		wipeProcess(prog)
		return run (directory, prog, numProcs, pargs, int (timeout) * 2)


def main ():
	program = "raytrace-scc"
	directory = "testingWB"
	timeout = 600

	#Parse options
	parser = OptionParser()
	parser.add_option("-a", "--arguments", dest="args", help="pacml program arguments", default ="")
	parser.add_option("-n", "--numProcs", dest="numProcs", help="number of processors", default =1)
	(options, args) = parser.parse_args()

	for n in [6, 8, 16, 32]:
		run(directory, program, n, options.args, timeout)

main ()
