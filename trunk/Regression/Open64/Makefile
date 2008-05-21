ifndef F90C
F90C=gfortran
endif

SOURCES=$(wildcard *.f90)
OBJS=$(SOURCES:.f90=.o)
BFILES=$(SOURCES:.f90=.B)
W2FFILES=$(SOURCES:.f90=.w2f.f)
W2FOBJS=$(SOURCES:.f90=.w2f.o)

all: $(OBJS) $(BFILES) $(W2FFILES) $(W2FOBJS)

%.o:%.f90
	$(F90C) -c $<

%.o:%.f
	$(F90C) -c $<

%.B:%.f90
	${OPEN64ROOT}/crayf90/sgi/mfef90 -F -z -N132 $<

%.w2f.f:%.B
	${OPEN64ROOT}/whirl2f/whirl2f -openad $<

clean: 
	rm -f $(OBJS) $(BFILES) $(W2FFILES) $(W2FOBJS) *.mod *.mod-whirl *.MOD
