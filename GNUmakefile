# to.cryp.blockio

.PHONY:	  all test clean distclean depend

GHC		= ghc
GHCFLAGS	= -Wall -O2 -funbox-strict-fields -i../streamproc
TESTINPUT	= test.data
TESTS		= wc-hgetbuf wc-lazy wc-blockio new-io wc-whilem-bytestring \
		  cat-bytestring cat-hgetbuf cat cat-malloc fast-io

CFLAGS		= -O3 -Wall -pedantic

RUNTEST		= env -i bash --noprofile --norc -c \
		  'ulimit -S -v 1048576; time -p $1 <$(TESTINPUT) >$(TESTINPUT).out'

% : %.hs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

% : %.lhs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

all:		$(TESTS)

test:		all $(TESTINPUT)
	$(call RUNTEST,/bin/cat)
	$(call RUNTEST,./cat-hgetbuf)
	$(call RUNTEST,./cat-bytestring)
	$(call RUNTEST,./cat)
	$(call RUNTEST,./cat-malloc)

#	$(call RUNTEST,/usr/bin/wc)
#	$(call RUNTEST,./wc-lazy)
#	$(call RUNTEST,./wc-hgetbuf)
#	$(call RUNTEST,./wc-blockio)
#	$(call RUNTEST,./wc-whilem-bytestring)
#	$(call RUNTEST,./new-io wcBuffer)
#	$(call RUNTEST,./new-io wcBufferST)
#	$(call RUNTEST,./new-io wcSlurpSP)
#	$(call RUNTEST,./new-io wcByteStrSP)

test.data:
	dd if=/dev/urandom of=$@ bs=1M count=1024

clean:
	@rm -f `find . \( -name '*.o' -o -name '*.hi' \)`
	@rm -f $(TESTS)

distclean:	clean
	@rm -f test.data

depend:
	${GHC} -M -optdep-f -optdepmakefile $(GHCFLAGS) `find . -name '*hs'`
