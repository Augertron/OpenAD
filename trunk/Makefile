# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/Makefile,v 1.1 2004-05-21 17:48:08 eraxxon Exp $
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

build: build_open64 build_oa build_xercesc build_openadforttk build_xaifbooster

build_open64:
	@echo "*** Building Open64 ***"
#	cd ${OPEN64ROOT}/crayf90/sgi ; $(MAKE)
#	cd ${OPEN64ROOT}/whirl2f ; $(MAKE)
#	cd ${OPEN64ROOT}/ir_tools ; $(MAKE)

build_oa:
	@echo "*** Building OA ***"
	cd ${OPENANALYSIS_BASE} ; $(MAKE)

build_xercesc:
	@echo "*** Building xercesc ***"
	cd ${XERCESC_BASE} ; $(MAKE)

build_openadforttk:
	@echo "*** Building OpenADFortTk ***"
	cd ${OPENADFORTTK}/src ; $(MAKE)

build_xaifbooster:
	@echo "*** Building xaifBooster ***"
	cd ${XAIFBOOSTER_BASE} ; $(MAKE)
	cd ${ANGEL_BASE} ; $(MAKE)
	cd ${XAIFBOOSTER_BASE} ; $(MAKE) test

clean:
	@echo "*** Not yet implemented ***"

.PHONY : configure build clean
.PHONY : build_open64 build_oa build_xercesc build_openadforttk \
	build_xaifbooster

############################################################

install: uninstall
	@echo "*** Installing (doing nothing) ***"

uninstall: 
	@echo "*** Uninstalling (doing nothing) ***"

.PHONY : install uninstall

#############################################################################

