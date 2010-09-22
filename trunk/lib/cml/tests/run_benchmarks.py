import os
import sys

def run(program, *args):
    pid = os.fork()
    if not pid:
        os.execvp(program, (program,) +  args)
    return os.wait()[0]

def runBenchmark(bench):
    prg = bench[0]
    arg = bench[1]
    print ("\nCompiling " + prg)
    run("mlton", prg + ".mlb")
    print ("\nRunning " + prg)
    run("./"+prg,arg)
    os.unlink(prg)
    return

benchmarks = [("primes","1000"),
              ("primes-multicast","1000"),
              ("mandelbrot", "100"),
              ("barnes-hut","4096"),
              ("life","100"),
              ("mergesort","10000"),
              ("ping-pong","1000000"),
              ("tsp","4"),
              ("tak","100"),
              ("matrix-multiply","2")]

for x in benchmarks:
    runBenchmark(x)



