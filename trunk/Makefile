# $Header: /Volumes/cvsrep/developer/OpenAD/Makefile,v 1.13 2007/04/11 13:28:12 utke Exp $

SHELL = /bin/sh
WD := $(shell pwd)

PLATFORM := $(shell cd $(WD)/config; ./hpcplatform)
ifeq ($(PLATFORM),)
  $(error "Unknown/unsupported platform") # unavailable in older gmakes
  error "Unknown/unsupported platform"    # will certainly cause an error
endif

# xercesc platform
ifeq ($(PLATFORM),x86-Cygwin)
  XERCESPLATFORM = CYGWIN
endif
ifeq ($(PLATFORM),x86-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),x86_64-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),ia64-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),opteron-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),sparc-SunOS)
  XERCESPLATFORM = SOLARIS
endif
ifndef XERCESPLATFORM
  $(error "Error: XERCESPLATFORM not set!")
endif

#############################################################################

# WARNING: exporting these causes problems with the Open64 build system
#export CC
#export CXX
#export CFLAGS
#export CXXFLAGS

#############################################################################

all: configure build 

.PHONY : all

#############################################################################

configure: 
	@echo "*** Configuring (doing nothing) ***"

build: open64_build oa_build xercesc_build openadforttk_build \
	angel_build xaifBooster_build

# clean build files only
clean: open64_clean oa_clean xercesc_clean openadforttk_clean \
	angel_clean xaifBooster_clean

# clean everything
veryclean: open64_veryclean oa_veryclean xercesc_veryclean \
	openadforttk_veryclean angel_veryclean xaifBooster_veryclean

.PHONY : configure build clean

#############################################################################

ifndef OPEN64ROOT
  $(error "Error: OPEN64ROOT not set!")
endif 

open64_build: open64_fe_build open64_be_build open64_tools_build

open64_fe_build: 
	cd $(OPEN64ROOT)/crayf90/sgi && $(MAKE) 

open64_be_build: 
	cd $(OPEN64ROOT)/whirl2f && $(MAKE)

open64_tools_build: 
	cd $(OPEN64ROOT)/ir_tools && $(MAKE)


open64_clean: 
	cd $(OPEN64ROOT) && $(MAKE) clean 

open64_veryclean: 
	cd $(OPEN64ROOT) && $(MAKE) clobber 

.PHONY : open64_build open64_fe_build open64_be_build open64_tools_build open64_clean open64_veryclean 

############################################################

# FIXME: make a rebuild target for OA and Xercesc Makefiles
# FIXME: reinstalling this stuff will cause exes like xaifBooster to relink!
OA_OPT = -f Makefile.quick CXX="$(CXX)"

oa_build:
	@if [ -d $(OPENANALYSIS_BASE) ]; then \
	  echo "*** Building OA ***" ; \
	  if [ -d $(OPENANALYSIS_BASE)/build-$(PLATFORM) ]; then \
	    cd $(OPENANALYSIS_BASE) && $(MAKE) $(OA_OPT) install ; \
	  else \
	    cd $(OPENANALYSIS_BASE) && $(MAKE) $(OA_OPT) all; \
	  fi \
	else \
	  echo "*** Building OA -- NON-EXISTENT ***" ; \
	fi

oa_clean:
	@if [ -d $(OPENANALYSIS_BASE) ]; then \
	  echo "*** Cleaning OA ***" ; \
	  cd $(OPENANALYSIS_BASE) && $(MAKE) $(OA_OPT) clean ; \
	else \
	  echo "*** Cleaning OA -- NON-EXISTENT ***" ; \
	fi

oa_veryclean: oa_clean

.PHONY : oa_build oa_clean oa_veryclean 

############################################################

XERCESC_OPT = CXX="$(CXX)" CC="$(CC)"

xercesc_build:
	@if [ -d $(XERCESC_BASE) ]; then \
	  echo "*** Building xercesc ***" ; \
	  if [ -d $(XERCESC_BASE)/xerces-c-src/obj/$(XERCESPLATFORM) ]; then \
	    cd $(XERCESC_BASE) && $(MAKE) $(XERCESC_OPT) build install ; \
	  else \
	    cd $(XERCESC_BASE) && $(MAKE) $(XERCESC_OPT) ; \
	  fi \
	else \
	  echo "*** Building xercesc -- NON-EXISTENT ***" ; \
	fi

xercesc_clean:
	@if [ -d $(XERCESC_BASE) ]; then \
	  echo "*** Cleaning xercesc ***" ; \
	  cd $(XERCESC_BASE) && $(MAKE) $(XERCESC_OPT) clean ; \
	else \
	  echo "*** Cleaning xercesc -- NON-EXISTENT ***" ; \
	fi

xercesc_veryclean: xercesc_clean

.PHONY : xercesc_build xercesc_clean xercesc_veryclean 

############################################################

FORTTK_OPT = -f Makefile.quick CXX="$(CXX)" CC="$(CC)" 

openadforttk_build:
	@if [ -d $(OPENADFORTTK_BASE) ]; then \
	  echo "*** Building OpenADFortTk ***" ; \
	  cd $(OPENADFORTTK_BASE) && $(MAKE) $(FORTTK_OPT) all; \
	else \
	  echo "*** Building OpenADFortTk -- NON-EXISTENT ***" ; \
	fi

openadforttk_clean:
	@if [ -d $(OPENADFORTTK_BASE) ]; then \
	  echo "*** Cleaning OpenADFortTk ***" ; \
	  cd $(OPENADFORTTK_BASE) && $(MAKE) $(FORTTK_OPT) clean ; \
	else \
	  echo "*** Cleaning OpenADFortTk -- NON-EXISTENT ***" ; \
	fi

openadforttk_veryclean: openadforttk_clean

.PHONY : openadforttk_build openadforttk_clean openadforttk_veryclean 

############################################################

angel_build:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Building angel ***" ; \
	  cd $(ANGEL_BASE) && $(MAKE) ; \
	else \
	  echo "*** Building angel -- NON-EXISTENT ***" ; \
	fi

angel_clean:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Cleaning angel (skipping) ***" ; \
	else \
	  echo "*** Cleaning angel -- NON-EXISTENT ***" ; \
	fi

angel_veryclean:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Very-Cleaning angel ***" ; \
	  cd $(ANGEL_BASE) && $(MAKE) clean ; \
	else \
	  echo "*** Very-Cleaning angel -- NON-EXISTENT ***" ; \
	fi

.PHONY : angel_build angel_clean angel_veryclean

############################################################

xaifBooster_build:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Building xaifBooster ***" ; \
	  cd $(XAIFBOOSTER_BASE) && $(MAKE) ; \
	else \
	  echo "*** Building xaifBooster -- NON-EXISTENT ***" ; \
	fi

xaifBooster_clean:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Cleaning xaifBooster (skipping) ***" ; \
	else \
	  echo "*** Cleaning xaifBooster -- NON-EXISTENT ***" ; \
	fi

xaifBooster_veryclean:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Very-Cleaning xaifBooster ***" ; \
	  cd $(XAIFBOOSTER_BASE) && $(MAKE) clean ; \
	else \
	  echo "*** Very-Cleaning xaifBooster -- NON-EXISTENT ***" ; \
	fi

.PHONY : xaifBooster_build xaifBooster_clean xaifBooster_veryclean

############################################################

ifndef INST_DIR
export INST_DIR=/opt/OpenAD
endif

install: uninstall open64_install openadforttk_install xaif_install xaifBooster_install
	cp -f README ${INST_DIR}
	cp -f README.License ${INST_DIR}
	cp -f openad_config.py ${INST_DIR}
	chmod a+r ${INST_DIR}/openad_config.py
	mkdir -p ${INST_DIR}/config
	cp -f config/hpcguess ${INST_DIR}/config
	cp -f config/config.guess ${INST_DIR}/config
	cp -f config/hpcplatform ${INST_DIR}/config
	chmod a+rx ${INST_DIR}/config/*
	mkdir -p ${INST_DIR}/bin
	cp -f bin/openad ${INST_DIR}/bin
	chmod a+rx ${INST_DIR}/bin/openad
	cp -rf runTimeSupport ${INST_DIR}
	chmod -R a+r ${INST_DIR}/runTimeSupport
	cp -f setenv.csh ${INST_DIR}
	chmod a+r ${INST_DIR}/setenv.csh
	cp -f setenv.sh ${INST_DIR}
	chmod a+r ${INST_DIR}/setenv.sh
	mkdir -p ${INST_DIR}/tools/setenv
	cp -f tools/setenv/setenv.py ${INST_DIR}/tools/setenv
	chmod a+r ${INST_DIR}/tools/setenv/setenv.py
	mkdir -p ${INST_DIR}/tools/libpythontk
	cp -f tools/libpythontk/*.py ${INST_DIR}/tools/libpythontk
	chmod a+r ${INST_DIR}/tools/libpythontk/*.py

open64_install: 
	cd Open64 && export INST_DIR=${INST_DIR}/Open64 && $(MAKE) install

FORTTK_INST_EXT=$(subst ${PWD},${INST_DIR}/OpenADFortTk,${OPENADFORTTKROOT})

openadforttk_install: 
# we do this here because of the not very usefull generated install inside FortTk
	mkdir -p ${FORTTK_INST_EXT}/bin
	cp -f ${OPENADFORTTKROOT}/bin/whirl2xaif ${FORTTK_INST_EXT}/bin/
	strip ${FORTTK_INST_EXT}/bin/whirl2xaif
	chmod a+rx ${FORTTK_INST_EXT}/bin/whirl2xaif
	cp -f ${OPENADFORTTKROOT}/bin/xaif2whirl ${FORTTK_INST_EXT}/bin/
	strip ${FORTTK_INST_EXT}/bin/xaif2whirl
	chmod a+rx ${FORTTK_INST_EXT}/bin/whirl2xaif
	mkdir -p ${INST_DIR}/OpenADFortTk/tools/multiprocess/perl/Lib/FT
	cp -f ${OPENADROOT}/OpenADFortTk/tools/multiprocess/multi-pp.pl ${INST_DIR}/OpenADFortTk/tools/multiprocess/
	chmod a+rx ${INST_DIR}/OpenADFortTk/tools/multiprocess/multi-pp.pl
	cp -f ${OPENADROOT}/OpenADFortTk/tools/multiprocess/Lib/*.pm ${INST_DIR}/OpenADFortTk/tools/multiprocess/perl/Lib
	chmod a+rx ${INST_DIR}/OpenADFortTk/tools/multiprocess/perl/Lib/*.pm
	cp -f ${OPENADROOT}/OpenADFortTk/tools/multiprocess/Lib/FT/*.pm ${INST_DIR}/OpenADFortTk/tools/multiprocess/perl/Lib/FT
	chmod a+rx ${INST_DIR}/OpenADFortTk/tools/multiprocess/perl/Lib/FT/*.pm

xaif_install: 
# we do this here because xaif does not have Makefiles
	mkdir -p ${INST_DIR}/xaif/schema/examples
	cp -f ${XAIFSCHEMAROOT}/schema/*.xsd ${INST_DIR}/xaif/schema
	chmod a+r ${INST_DIR}/xaif/schema/*.xsd
	cp -f ${XAIFSCHEMAROOT}/schema/examples/inlinable_intrinsics.xaif ${INST_DIR}/xaif/schema/examples
	chmod a+r ${INST_DIR}/xaif/schema/examples/inlinable_intrinsics.xaif

xaifBooster_install: 
	cd xaifBooster && export INST_DIR=${INST_DIR}/xaifBooster && $(MAKE) install

uninstall: 
	@if [ -d ${INST_DIR} ]; then \
	  echo "about to uninstall ${INST_DIR}"; \
	  rm -rI ${INST_DIR}; \
        fi

.PHONY : install uninstall open64_install openadforttk_install xaif_install xaifBooster_install

#############################################################################
