SHELL = /bin/sh
WD := $(shell pwd)

PLATFORM := $(shell cd $(WD)/config; ./hpcplatform)
ifeq ($(PLATFORM),)
  $(error "Unknown/unsupported platform")
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

build: \
build_Open64 \
build_OpenAnalysis \
build_xercesc \
build_OpenADFortTk \
build_angel \
build_xaifBooster

# clean build files only
clean: \
clean_Open64 \
clean_OpenAnalysis \
clean_xercesc \
clean_OpenADFortTk \
clean_angel \
clean_xaifBooster

# clean everything
veryclean: \
veryclean_Open64 \
veryclean_OpenAnalysis \
veryclean_xercesc \
veryclean_OpenADFortTk \
veryclean_angel \
veryclean_xaifBooster

.PHONY : configure build clean veryclean

#############################################################################

ifndef OPEN64ROOT
  $(error "Error: OPEN64ROOT not set!")
endif 

build_Open64: build_Open64_fe build_Open64_be build_Open64_tools

build_Open64_fe: 
	cd $(OPEN64ROOT)/crayf90/sgi && $(MAKE) 

build_Open64_be: 
	cd $(OPEN64ROOT)/whirl2f && $(MAKE)

build_Open64_tools: 
	cd $(OPEN64ROOT)/ir_tools && $(MAKE)


clean_Open64: 
	cd $(OPEN64ROOT) && $(MAKE) clean 

veryclean_Open64: 
	cd $(OPEN64ROOT) && $(MAKE) clobber 

.PHONY : build_Open64 build_Open64_fe build_Open64_be build_Open64_tools clean_Open64 veryclean_Open64 

############################################################

OA_OPT = -f Makefile.quick CXX="$(CXX)"

build_OpenAnalysis:
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

clean_OpenAnalysis:
	@if [ -d $(OPENANALYSIS_BASE) ]; then \
	  echo "*** Cleaning OA ***" ; \
	  cd $(OPENANALYSIS_BASE) && $(MAKE) $(OA_OPT) clean ; \
	else \
	  echo "*** Cleaning OA -- NON-EXISTENT ***" ; \
	fi

veryclean_OpenAnalysis: clean_OpenAnalysis

.PHONY : build_OpenAnalysis clean_OpenAnalysis veryclean_OpenAnalysis 

############################################################

XERCESC_OPT = CXX="$(CXX)" CC="$(CC)"

build_xercesc:
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

clean_xercesc:
	@if [ -d $(XERCESC_BASE) ]; then \
	  echo "*** Cleaning xercesc ***" ; \
	  cd $(XERCESC_BASE) && $(MAKE) $(XERCESC_OPT) clean ; \
	else \
	  echo "*** Cleaning xercesc -- NON-EXISTENT ***" ; \
	fi

veryclean_xercesc: clean_xercesc

.PHONY : build_xercesc clean_xercesc veryclean_xercesc 

############################################################

FORTTK_OPT = -f Makefile.quick CXX="$(CXX)" CC="$(CC)" 

build_OpenADFortTk:
	@if [ -d $(OPENADFORTTK_BASE) ]; then \
	  echo "*** Building OpenADFortTk ***" ; \
	  cd $(OPENADFORTTK_BASE) && $(MAKE) $(FORTTK_OPT) all; \
	else \
	  echo "*** Building OpenADFortTk -- NON-EXISTENT ***" ; \
	fi

clean_OpenADFortTk:
	@if [ -d $(OPENADFORTTK_BASE) ]; then \
	  echo "*** Cleaning OpenADFortTk ***" ; \
	  cd $(OPENADFORTTK_BASE) && $(MAKE) $(FORTTK_OPT) clean ; \
	else \
	  echo "*** Cleaning OpenADFortTk -- NON-EXISTENT ***" ; \
	fi

veryclean_OpenADFortTk: clean_OpenADFortTk

.PHONY : build_OpenADFortTk clean_OpenADFortTk veryclean_OpenADFortTk 

############################################################

build_angel:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Building angel ***" ; \
	  cd $(ANGEL_BASE) && $(MAKE) ; \
	else \
	  echo "*** Building angel -- NON-EXISTENT ***" ; \
	fi

clean_angel:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Cleaning angel (skipping) ***" ; \
	else \
	  echo "*** Cleaning angel -- NON-EXISTENT ***" ; \
	fi

veryclean_angel:
	@if [ -d $(ANGEL_BASE) ]; then \
	  echo "*** Very-Cleaning angel ***" ; \
	  cd $(ANGEL_BASE) && $(MAKE) clean ; \
	else \
	  echo "*** Very-Cleaning angel -- NON-EXISTENT ***" ; \
	fi

.PHONY : build_angel clean_angel veryclean_angel

############################################################

build_xaifBooster:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Building xaifBooster ***" ; \
	  cd $(XAIFBOOSTER_BASE) && $(MAKE) ; \
	else \
	  echo "*** Building xaifBooster -- NON-EXISTENT ***" ; \
	fi

clean_xaifBooster:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Cleaning xaifBooster (skipping) ***" ; \
	else \
	  echo "*** Cleaning xaifBooster -- NON-EXISTENT ***" ; \
	fi

veryclean_xaifBooster:
	@if [ -d $(XAIFBOOSTER_BASE) ]; then \
	  echo "*** Very-Cleaning xaifBooster ***" ; \
	  cd $(XAIFBOOSTER_BASE) && $(MAKE) clean ; \
	else \
	  echo "*** Very-Cleaning xaifBooster -- NON-EXISTENT ***" ; \
	fi

.PHONY : build_xaifBooster clean_xaifBooster veryclean_xaifBooster

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
