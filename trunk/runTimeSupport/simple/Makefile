ifndef F90C
F90C=gfortran
FC=gfortran
endif

EXTERNAL=w2f__types.f90 OAD_active.f90

OBJ=$(EXTERNAL:.f90=.o) OAD_cp.o OAD_tape.o ad_inline.o 

test: $(OBJ)

OAD_active.f90: ../scalar/OAD_active.f90
	ln -sf $< $@

w2f__types.f90: ../all/w2f__types.f90
	ln -sf $< $@

%.o: %.f90
	$(F90C) -c $<

%.o: %.f 
	$(FC) -c $< 

clean: 
	rm -f *.o *.mod $(EXTERNAL)

.PHONY: clean
