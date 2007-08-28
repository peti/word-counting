# to.cryp.blockio

.PHONY:	  all test clean distclean depend

GHC       := ghc
GHCFLAGS  := -Wall -O2 -funbox-strict-fields -i../streamproc
TESTINPUT := test.data # usr/share/dict/words
TESTS     := wc-hgetbuf wc-lazy wc-blockio new-io wc-whilem-bytestring

% : %.hs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

% : %.lhs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

all:		$(TESTS)

test:		all $(TESTINPUT)
	time /usr/bin/wc <$(TESTINPUT)
#	time ./wc-lazy <$(TESTINPUT)
	time ./wc-hgetbuf <$(TESTINPUT)
	time ./wc-blockio <$(TESTINPUT)
	time ./wc-whilem-bytestring <$(TESTINPUT)
	time ./new-io wcBuffer <$(TESTINPUT)
	time ./new-io wcBufferST <$(TESTINPUT)
	time ./new-io wcSlurpSP <$(TESTINPUT)
	time ./new-io wcByteStrSP <$(TESTINPUT)

test.data:
	dd if=/dev/urandom of=$@ bs=1M count=512

clean:
	@rm -f `find . \( -name '*.o' -o -name '*.hi' \)`
	@rm -f $(TESTS)

distclean:	clean
	@rm -f test.data

depend:
	${GHC} -M -optdep-f -optdepmakefile $(GHCFLAGS) `find . -name '*hs'`
