VARIANTS=scalar scalarNDI vector trace

all: $(addsuffix /OAD_active.f90, $(addprefix ../, $(VARIANTS)))

CPPFLAGS=-traditional -P

../scalar/OAD_active.f90: OAD_active.F90
	$(CPP) $(CPPFLAGS) -o $@ $<

../scalarNDI/OAD_active.f90: OAD_active.F90
	$(CPP) $(CPPFLAGS) -DSCALARNDI -o $@ $<

../vector/OAD_active.f90: OAD_active.F90
	$(CPP) $(CPPFLAGS) -DVECTOR -o $@ $<

../trace/OAD_active.f90: OAD_active.F90
	$(CPP) $(CPPFLAGS) -DTRACE -o $@ $<
