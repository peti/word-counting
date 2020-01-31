# GNUmakefile

.PHONY:   all test clean distclean depend

GHC             = ghc
GHCFLAGS        = -Wall -O2 -funbox-strict-fields

RANDOM_DEVICE  := $(word 1, $(wildcard /dev/frandom /dev/urandom))
TESTDATA        = /dev/shm/test.data
TESTDATA_SIZE   = 15 # in GByte

TESTS           = cat-bytestring cat-hgetbuf

RUNTEST         = time -p env -i PATH=$$PATH $1 <$(TESTDATA) >/dev/null

% : %.hs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

% : %.lhs
	${GHC} ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

all:		$(TESTS)

test:		all $(TESTDATA)
	@echo "##### POSIX cat with 32KB i/o buffer"
	@echo ""
	$(call RUNTEST,cat)
	@echo ""
	@echo "##### Haskell cat with 32KB i/o buffer"
	@echo ""
	$(call RUNTEST,./cat-hgetbuf 32768)
	$(call RUNTEST,./cat-bytestring 32768)
	@echo ""
	@echo "##### Haskell cat with 128KB i/o buffer"
	@echo ""
	$(call RUNTEST,./cat-hgetbuf 131072)
	$(call RUNTEST,./cat-bytestring 131072)
	@echo ""
	@echo "##### Haskell cat with 1GB i/o buffer"
	@echo ""
	$(call RUNTEST,./cat-hgetbuf 1048576)
	$(call RUNTEST,./cat-bytestring 1048576)

$(TESTDATA):
	dd if=$(RANDOM_DEVICE) of=$@ bs=1G count=$(TESTDATA_SIZE)

clean:
	@rm -f *.o *.hi
	@rm -f $(TESTS)

distclean:	clean
	@rm -f $(TESTDATA)

depend:
	${GHC} -M -dep-makefile GNUmakefile $(GHCFLAGS) `find . -name '*hs'`
