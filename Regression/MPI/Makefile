MPICHDIR = ../..
FC       = $(MPIROOT)/bin/mpif90
FFLAGS   = -g -O0

all: driver driverAdm

driver: driver.o compute.o init.o
	$(FC) -o $@ $^ $(LDFLAGS) $(LIBS)

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

%.f90 : %.F90
	$(FC) $(FFLAGS) -o $@ -E $< 

driverAdm: w2f__types.o OADactive.o OADtape.o OADrev.o driverAdm.o compute.xb.x2w.w2f.pp.o init.o
	$(FC) -o $@ $^ $(LDFLAGS) $(LIBS)

# F -> WHIRL
compute.B: compute.F90
	${OPEN64ROOT}/crayf90/sgi/mfef90 -DOAD_TRANS -z -I$(MPIROOT)/include -F -N132 $<

# WHIRL -> XAIF
compute.xaif : compute.B 
	${OPENADFORTTKROOT}/bin/whirl2xaif -o $@ $<

# XAIF -> XAIF'
compute.xb.xaif : compute.xaif
	${XAIFBOOSTERROOT}/xaifBooster/algorithms/BasicBlockPreaccumulationReverse/test/t -i $< -c ${XAIFSCHEMAROOT}/schema/examples/inlinable_intrinsics.xaif -s ${XAIFSCHEMAROOT}/schema -o $@ -U

# XAIF' -> WHIRL'
compute.xb.x2w.B : compute.xb.xaif  
	${OPENADFORTTKROOT}/bin/xaif2whirl --structured compute.B $<

# WHIRL' -> F'
compute.xb.x2w.w2f.f: compute.xb.x2w.B
	${OPEN64ROOT}/whirl2f/whirl2f -openad $<

# postprocess F'
compute.xb.x2w.w2f.pp.f: compute.xb.x2w.w2f.f
	perl ${OPENADFORTTKROOT}/bin/multi-pp.pl $<

clean:
	rm -f *.o driver driverAdm compute.x* compute.B *.mod *.mod-whirl
