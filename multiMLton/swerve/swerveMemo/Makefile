
TESTDIRS = common/tests config/tests/ ietf/tests/ server/tests/
CODEDIRS = common config ietf main server store $(TESTDIRS)

#  REVISIT - work out the sml architecture automatically
ARCH	= x86-linux

all:
	cd main; echo "CM.make()" | sml

profile:
	cd main; echo "Compiler.Profile.setProfMode true; CM.make()" | sml

test:
	for d in $(TESTDIRS); do (cd $$d; echo "CM.make()" | sml); done

clean:
	for d in $(CODEDIRS); do $(RM) -r $$d/CM; done

veryclean: clean
	for d in $(TESTDIRS); do $(RM) $$d/*.$(ARCH); done
	$(RM) main/swerve.$(ARCH)

