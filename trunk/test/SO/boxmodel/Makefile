# ========== begin copyright notice ==============
# This file is part of 
# ---------------
# xaifBooster
# ---------------
# Distributed under the BSD license as follows:
# Copyright (c) 2005, The University of Chicago
# All rights reserved.
#
# Redistribution and use in source and binary forms, 
# with or without modification, are permitted provided that the following conditions are met:
#
#    - Redistributions of source code must retain the above copyright notice, 
#      this list of conditions and the following disclaimer.
#    - Redistributions in binary form must reproduce the above copyright notice, 
#      this list of conditions and the following disclaimer in the documentation 
#      and/or other materials provided with the distribution.
#    - Neither the name of The University of Chicago nor the names of its contributors 
#      may be used to endorse or promote products derived from this software without 
#      specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
# SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# General Information:
# xaifBooster is intended for the transformation of 
# numerical programs represented as xml files according 
# to the XAIF schema. It is part of the OpenAD framework. 
# The main application is automatic 
# differentiation, i.e. the generation of code for 
# the computation of derivatives. 
# The following people are the principal authors of the 
# current version: 
# 	Uwe Naumann
#	Jean Utke
# Additional contributors are: 
#	Andrew Lyons
#	Peter Fine
#
# For more details about xaifBooster and its use in OpenAD please visit:
#   http://www.mcs.anl.gov/openad
#
# This work is partially supported by:
# 	NSF-ITR grant OCE-0205590
# ========== end copyright notice ==============
######################################################
# pick your compiler
######################################################
#FIXEDFORMAT=-ffixed-form
#FREEFORMAT=-ffree-form
#F95=g95 -g -O0 
#CC=gcc

#FIXEDFORMAT=-ffixed-form
#FREEFORMAT=-ffree-form
#F95=gfortran -g -O0 
#CC=gcc

FIXEDFORMAT=-fixed
FREEFORMAT=-free
F95=ifort  -g -fpe0 -O0 -traceback 
CC=icc

#FIXEDFORMAT=-fixed
#FREEFORMAT=-free
#F95=f95 -g -w=unused
#CC=gcc
######################################################

REV_MODULES=                    \
w2f__types.o                    \
OpenAD_active.o                 \
OpenAD_checkpoints.o            \
OpenAD_rev.o                    \
OpenAD_tape.o                   \
iaddr.o                         

FW_PP_FLAGS=-f


all: run

ifndef OPENADROOT
  $(error "Error:  environment variable OPENADROOT not defined!")
endif

ifndef XAIFSCHEMAROOT
  $(error "Error:  environment variable XAIFSCHEMAROOT not defined!")
endif

ifndef XAIFBOOSTERROOT
  $(error "Error:  environment variable XAIFBOOSTERROOT not defined!")
endif

LN=ln -sf

RM=rm -rf

######################################################
# transformation part 1
######################################################

# paste the globals module into the head file 
# so we can do the single file transformation
head_sf.f : all_globals_mod.f head.f 
	cat $^ > $@

# F -> WHIRL
head_sf.B: head_sf.f mfef90
	./mfef90 -F -N132 $<

# WHIRL -> XAIF
head_sf.xaif : head_sf.B whirl2xaif
	./whirl2xaif -n $< | sed 's/prefix=\"OpenAD_\"/prefix="oad"/' > $@ 

# XAIF -> XAIF'
head_sf.xb.xaif : head_sf.xaif xaif.xsd xaif_base.xsd xaif_inlinable_intrinsics.xsd xaif_derivative_propagator.xsd xaif_output.xsd openad_tlm
	./openad_tlm -i $< -c ${XAIFSCHEMAROOT}/schema/examples/inlinable_intrinsics.xaif -o $@ 

# XAIF' -> WHIRL'
head_sf.xb.x2w.B : head_sf.xb.xaif xaif2whirl
	./xaif2whirl --structured head_sf.B $<

# WHIRL' -> F'
head_sf.xb.x2w.w2f.f: head_sf.xb.x2w.B whirl2f whirl2f_be
	./whirl2f -openad $<

# postprocess F'
head_sf.xb.x2w.w2f.pp.f: head_sf.xb.x2w.w2f.f multi-pp1.pl
	perl multi-pp1.pl $(FW_PP_FLAGS) $<
	cat head_sf.xb.x2w.w2f.pp.f | sed '/OPENAD .*DEPENDENT/d' > $@

# extract the transformed globals module so we the right order of 
# compilation with respect to the globals checkpoint module
all_globals_mod.once.f: head_sf.xb.x2w.w2f.pp.f
	cat $< | sed -n '/MODULE all_globals_mod/,/END MODULE/p' > $@

# remove the transformed globals module from the transformed head file
head.once.f: head_sf.xb.x2w.w2f.pp.f
	cat $< | sed '/MODULE all_globals_mod/,/END MODULE/d' > $@

######################################################
# transformation part 2
######################################################

# paste in the array propagators 
head.once_sf.f : arrayProp.f all_globals_mod.once.f head.once.f driver_g.f
	cat $^ > $@

w2f__types.B: w2f__types.f90 mfef90
	./mfef90 -F -N132 $<

# F -> WHIRL
head.once_sf.B:  head.once_sf.f w2f__types.B mfef90
	./mfef90 -F -N132 $<

# WHIRL -> XAIF
head.once_sf.xaif : head.once_sf.B whirl2xaif
	./whirl2xaif -n -o $@ $<  

# XAIF -> XAIF'
head.once_sf.xb.xaif : head.once_sf.xaif xaif.xsd xaif_base.xsd xaif_inlinable_intrinsics.xsd xaif_derivative_propagator.xsd xaif_output.xsd openad_adm
	./openad_adm -i $< -c ${XAIFSCHEMAROOT}/schema/examples/inlinable_intrinsics.xaif -o $@ 

# XAIF' -> WHIRL'
head.once_sf.xb.x2w.B : head.once_sf.xb.xaif xaif2whirl
	./xaif2whirl --structured head.once_sf.B $<

# WHIRL' -> F'
head.once_sf.xb.x2w.w2f.f: head.once_sf.xb.x2w.B whirl2f whirl2f_be
	./whirl2f -openad $<

# postprocess F'
head.once_sf.xb.x2w.w2f.pp.f: head.once_sf.xb.x2w.w2f.f multi-pp.pl ad_template.f ad_inline.f
	perl multi-pp.pl $<

# extract the transformed globals module so we the right order of 
# compilation with respect to the globals checkpoint module
all_globals_mod.twice.f: head.once_sf.xb.x2w.w2f.pp.f
	cat $< | sed -n '/MODULE all_globals_mod/,/END MODULE/p' > $@

# remove the transformed globals module from the transformed head file
head.twice.f: head.once_sf.xb.x2w.w2f.pp.f
	cat $< | sed '/MODULE all_globals_mod/,/END MODULE/d' > $@

######################################################
# links to OpenAD tools and schemata
######################################################

%.xsd:
	$(LN) ${XAIFSCHEMAROOT}/schema/$@ .

mfef90: 
	$(LN) ${OPENADROOT}/Open64/osprey1.0/targ_ia32_ia64_linux/crayf90/sgi/mfef90 .

whirl2xaif xaif2whirl whirl2sexp sexp2whirl: 
	$(LN) ${OPENADROOT}/OpenADFortTk/OpenADFortTk-x86-Linux/bin/$@ .

%.pl: 
	$(LN) ${OPENADROOT}/OpenADFortTk/tools/multiprocess/$@ .

whirl2f whirl2f_be:
	$(LN) ${OPENADROOT}/Open64/osprey1.0/targ_ia32_ia64_linux/whirl2f/$@ .

openad_tlm:
	$(LN) ${OPENADROOT}/xaifBooster/algorithms/BasicBlockPreaccumulation/test/t $@

openad_adm:
	$(LN) ${OPENADROOT}/xaifBooster/algorithms/BasicBlockPreaccumulationReverse/test/t $@

w2f__types.f90 iaddr.c: 
	$(LN) ../../../runTimeSupport/all/$@ .

OpenAD_active.f90 OpenAD_checkpoints.f90 OpenAD_dct.f90 OpenAD_rev.f90 OpenAD_tape.f90 :
	$(LN) ../../../runTimeSupport/simple/$@ .

ad_template.f: ../../../runTimeSupport/simple/ad_template.split.f
	cp $< $@

ad_inline.f: ../../../runTimeSupport/simple/ad_inline.f
	cp $< $@

######################################################
# compilation
######################################################

#  function driver 
main_f: all_globals_mod.o head.o main_f.o  
	$(F95) $^ -o $@ 

#  gradient driver 
main_g: w2f__types.o all_globals_mod.once.o head.once.o driver_g.o main_g.o  arrayProp.o  
	$(F95) $^ -o $@ 

#  hessian driver 
main_h: $(REV_MODULES) all_globals_mod.twice.o head.twice.o driver_h.o main_h.o  
	$(F95) $^ -o $@ 

numericalComparison: numericalComparison.o
	$(F95) $^ -o $@ 

run: main_f main_g main_h
	./main_f
	./main_g
	./main_h

%.o:%.f
	$(F95) $(FIXEDFORMAT) -c $<

%.o:%.f90
	$(F95) $(FREEFORMAT) -c $<


######################################################
# miscellaneous
######################################################

tmpOutput: 
	mkdir -p $@


clean: 
	$(RM) all_globals_mod.[^f] *once* *twice* main_f main_g main_h head_sf.*  *.B *.mod-whirl ad_template.f ad_inline.f
	$(RM) *.mod *.o
	-find . -type l -exec ${RM} {} \;

obj test doc: 

.PHONY: all obj test doc clean run
