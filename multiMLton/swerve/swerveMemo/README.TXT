History
=======
During my random explorations with various languages and compilers I spent some time with SML.  While coding personal project I felt I needed to embed a http server for web based configuration and administration.  The SML code pool is know for its paucity as well as its quality, so I was surprised to find a rather full featured implementation.

Over the next 2-3 weeks I ported it to compile under MLton.  The original author had a rather idiomatic style towards his SML coding conventions and I performed a few refactors of the pure cut and paster variety while picking off compiler errors.

Eventually I achieved an unexpected clean compile and quickly determined that it:
	- runs
	- reads it configuration file
	- correctly served a static html page
	- correctly served a gif image

at which point a promptly stopped and moved on to something new and drifted away from SML alltogether. [Still love the language.]

Six months later, June '05, I stumbled across the project there it was still sitting on my hard drive, during a rainy Saturday and figured what the hell, zip it up and throw it out there.  Who knows someone, somewhere at sometime may find a bit of use for it.

Caveats
=======
Total testing of this port consisted of a successful compile, startup, and serving one html page with one gif image.  Given all that the original code was throughly designed and implemented ina thought full manner and I expect is quite usable with an few minor bugs introduced by my porting effort.


Known Issues
============

In finalizse.sml weak references to open file resources was removed and should be fixed.See source file for fix-me.

Building and Running
====================
See BUILD.TXT



Ray Racine
rracine (aatt) adelphia (ddoott) net
Boca Raton, FL
June 18, 2005
