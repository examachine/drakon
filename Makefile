# Makefile for GNU make
# Eray Ozkural aka exa
# hacked a bit from edison's makefile

# dirs

subdirs		= .

builddir	= out

# files

progs = test-c++-parser
#progs = stockdb2txnset test-sessiondb test-stockdb

# more dirs

VPATH		= $(subdirs) $(importdir) $(outdir)

fullsources     = $(foreach dir, $(subdirs), $(wildcard $(dir)/*.hs))
sources         = $(foreach file, $(fullsources), $(notdir $(file)))
objects         = $(foreach file, $(sources:.hs=.o), $(file))

# tools and options

ghc		= ghc
#ghcflags	= -prof -auto-all \
#	-package data -package lang -package posix
ghcflags	= -package data -package text -fglasgow-exts
# -package posix

# -cpp -i$(importdir)
#-O2 -fvia-C
default: all

$(progs): % : %.hs
	$(ghc) --make $(ghcflags) $< -o $@

$(progs): $(fullsources) # we use --make

all: checkdir $(progs)

checkdir:
	@-mkdir $(builddir)

clean:
	rm -f *.o *.hi $(progs)

#check: test-malfunctor
#	test-malfunctor malcircle.raw
#	sox -V -r 22500 -w -s malcircle.raw malcircle.wav
#	play malcircle.wav
#test-sine
#	test-sine test.raw
#	sox -V -r 22500 -w -s test.raw test.wav
#	play test.wav

.PHONY:	clean checkdir check depend

#epend:
#	ghc -M  $(fullsources)
