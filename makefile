# to.cryp.blockio

.PHONY:	  all test clean distclean depend

GHC       := ghc
GHCFLAGS  := -Wall -O2 -funbox-strict-fields
TESTINPUT := test.data # usr/share/dict/words
TESTS     := tutorial wc-hgetbuf wc-lazy wc-blockio iodriver new-io

% : %.hs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

% : %.lhs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

%.html : %.lhs
	lhs2html $<

%.hs : %.hsc
	hsc2hs $<

all:		$(TESTS)

test:		all $(TESTINPUT)
#	time /usr/bin/wc <$(TESTINPUT)
#	time ./tutorial wcLazy <$(TESTINPUT)
#	time ./wc-lazy <$(TESTINPUT)
	time ./wc-hgetbuf <$(TESTINPUT)
#	time ./wc-blockio <$(TESTINPUT)
#	time ./tutorial wcHandle <$(TESTINPUT)
#	time ./tutorial wc <$(TESTINPUT)
#	time ./tutorial lrev <$(TESTINPUT) >/dev/null
#	time ./tutorial lrev </etc/profile
#	time ./tutorial lrevLazy <$(TESTINPUT) >/dev/null
#	time ./iodriver <$(TESTINPUT) >/dev/null
	time ./new-io <$(TESTINPUT) >/dev/null

iodriver:	Types.hs

test.data:
	dd if=/dev/urandom of=$@ bs=1M count=512

clean:
	@rm -f `find . \( -name '*.o' -o -name '*.hi' \)`
	@rm -f tutorial.html $(TESTS)

distclean:	clean
	@rm -f Types.hs test.data

depend:
	${GHC} -M -optdep-f -optdepmakefile $(GHCFLAGS) `find . -name '*.hs'`
