# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/Makefile,v 1.2 2004-06-01 21:10:23 eraxxon Exp $
# -*-makefile-*-
## * BeginCopyright *********************************************************
## 
## 
## *********************************************************** EndCopyright *

#############################################################################
# $Source: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/Makefile,v $
#############################################################################

#############################################################################

SHELL = /bin/sh
WD := $(shell pwd)

PLATFORM := $(shell cd $(WD)/config; ./hpcplatform)
ifeq ($(PLATFORM),)
  $(error "Unknown/unsupported platform") # unavailable in older gmakes
  error "Unknown/unsupported platform"    # will certainly cause an error
endif

# xercesc platform
ifeq ($(PLATFORM),i686-Cygwin)
  XERCESPLATFORM = CYGWIN
endif
ifeq ($(PLATFORM),i686-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),ia64-Linux)
  XERCESPLATFORM = LINUX
endif
ifeq ($(PLATFORM),opteron-Linux)
  XERCESPLATFORM = LINUX
endif
ifndef XERCESPLATFORM
  $(error "Error: XERCESPLATFORM not set!")
endif

#############################################################################

# used by make and configure (from GNU Autoconf)
export CC
export CXX
export CFLAGS
export CXXFLAGS

#############################################################################

all: configure build install

.PHONY : all

#############################################################################

configure: 
	@echo "*** Configuring (doing nothing) ***"

build: open64_build oa_build xercesc_build openadforttk_build \
	xaifbooster_build

# clean build files only
clean: open64_clean oa_clean xercesc_clean openadforttk_clean \
	xaifbooster_clean

# clean everything
veryclean: open64_veryclean oa_veryclean xercesc_veryclean \
	openadforttk_veryclean xaifbooster_veryclean

.PHONY : configure build clean

#############################################################################

open64_build:
	@echo "*** Building Open64 ***"
# FIXME: build dies when launched from this makefile!?
#	cd $(OPEN64ROOT)/crayf90/sgi ; $(MAKE)
#	cd $(OPEN64ROOT)/whirl2f ; $(MAKE)
#	cd $(OPEN64ROOT)/ir_tools ; $(MAKE)

open64_clean:
	@echo "*** Cleaning Open64 ***"
	cd $(OPEN64ROOT) ; $(MAKE) clean

open64_veryclean:
	@echo "*** Very-Cleaning Open64 ***"
	cd $(OPEN64ROOT) ; $(MAKE) clobber


# FIXME: make a rebuild target for OA and Xercesc Makefiles
# FIXME: reinstalling this stuff will cause exes like xaifbooster to relink!
oa_build:
	@echo "*** Building OA ***"
	if [ -d $(OPENANALYSIS_BASE)/build-$(PLATFORM) ]; then \
	  cd $(OPENANALYSIS_BASE) ; $(MAKE) build install ; \
	else \
	  cd $(OPENANALYSIS_BASE) ; $(MAKE) ; \
	fi

oa_clean:
	@echo "*** Cleaning OA ***"
	cd $(OPENANALYSIS_BASE) ; $(MAKE) clean

oa_veryclean: oa_clean


xercesc_build:
	@echo "*** Building xercesc ***"
	if [ -d $(XERCESC_BASE)/xerces-c-src_2_3_0/obj/$(XERCESPLATFORM) ]; then \
	  cd $(XERCESC_BASE) ; $(MAKE) build install ; \
	else \
	  cd $(XERCESC_BASE) ; $(MAKE) ; \
	fi

xercesc_clean:
	@echo "*** Cleaning xercesc ***"
	cd $(XERCESC_BASE) ; $(MAKE) clean

xercesc_veryclean: xercesc_clean


openadforttk_build:
	@echo "*** Building OpenADFortTk ***"
	cd $(OPENADFORTTK)/src ; $(MAKE)

openadforttk_clean:
	@echo "*** Cleaning OpenADFortTk -- SKIPPING ***"

openadforttk_veryclean:
	@echo "*** Very-Cleaning OpenADFortTk ***"
	cd $(OPENADFORTTK)/src ; $(MAKE) veryclean


xaifbooster_build:
	@echo "*** Building xaifBooster ***"
	cd $(XAIFBOOSTER_BASE) ; $(MAKE)
	cd $(ANGEL_BASE) ; $(MAKE)
	cd $(XAIFBOOSTER_BASE) ; $(MAKE) test

xaifbooster_clean:
	@echo "*** Cleaning xaifBooster -- SKIPPING ***"

xaifbooster_veryclean:
	@echo "*** Very-Cleaning xaifBooster ***"
	cd $(ANGEL_BASE) ; $(MAKE) clean
	cd $(XAIFBOOSTER_BASE) ; $(MAKE) clean


.PHONY : open64_build open64_clean open64_veryclean \
	oa_build oa_clean oa_veryclean \
	xercesc_build xercesc_clean xercesc_veryclean \
	openadforttk_build openadforttk_clean openadforttk_veryclean \
	xaifbooster_build xaifbooster_clean xaifbooster_veryclean

############################################################

install: uninstall
	@echo "*** Installing (doing nothing) ***"

uninstall: 
	@echo "*** Uninstalling (doing nothing) ***"

.PHONY : install uninstall

#############################################################################

