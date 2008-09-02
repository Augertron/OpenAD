#!/usr/bin/env python

import os
import sys
import subprocess
import string

#############################################################################
##
## OpenAD configuration information for subpackages.
##
## This is a Python module masquerading as a configuration file.
## Figuring this out should be pretty easy, but if you are making
## changes to the defaults, we presume you know what you are doing or
## can handle the consequences.
##
##############################################################################
#Get path of file's directory (it should be called from the directory it's in)
OpenADRoot=os.path.dirname(__file__)
#Calling script should be located in OpenAD root directory
sys.path.append(os.path.join(OpenADRoot,'tools','libpythontk'))
import RunCmds
import Repository

class openad_config:
  ''' A list of all repositories in this configuation of OpenAD '''
  def __init__(self):
    self.OpenADRepos = {}
    riceSvnUrl = 'http://hpc.svn.rice.edu/r/'
    # Open64
    self.OpenADRepos["Open64"]=Repository.SVNRepository(riceSvnUrl+'open64/tags/version-openad',OpenADRoot,'Open64',None,None,"OPEN64_BASE")
    # OpenADFortTk
    self.OpenADRepos["OpenAdFortTk"]=Repository.SVNRepository(riceSvnUrl+'OpenADFortTk/trunk',OpenADRoot,'OpenADFortTk',None,None,'OPENADFORTTK_BASE') 
    # OpenAnalysis
    self.OpenADRepos["OpenAnalysis"]=Repository.SVNRepository(riceSvnUrl+'open-analysis/tags/version-openad',OpenADRoot,'OpenAnalysis',None,None,'OPENANALYSIS_BASE')
    # xercesc
    self.OpenADRepos["xercesc"]=Repository.SVNRepository(riceSvnUrl+'xercesc/tags/version-openad',OpenADRoot,'xercesc', None, None,'XERCESC_BASE')
    # xaifBooster
    self.OpenADRepos["xaifBooster"]=Repository.SVNRepository(riceSvnUrl+'xaifBooster/trunk',OpenADRoot,'xaifBooster',None,None,'XAIFBOOSTER_BASE')
    # xaif
    self.OpenADRepos["xaif"]=Repository.SVNRepository(riceSvnUrl+'xaif/trunk',OpenADRoot,'xaif',None,None,'XAIFSCHEMA_BASE')
    # angel
    self.OpenADRepos["angel"]=Repository.CVSRepository('pserver',':pserver:anonymous@angellib.cvs.sourceforge.net:/cvsroot/angellib',OpenADRoot,'angel',None,None,'ANGEL_BASE')
    # boost
    self.OpenADRepos["boost"]=Repository.CVSRepository('pserver',':pserver:anonymous@boost.cvs.sourceforge.net:/cvsroot/boost',OpenADRoot,'boost','boost','Version_1_34_1','BOOST_BASE')

    self.setPythonOpenADEnvVars()
    
    platformToOpen64TargTable = {'alpha-OSFI'  : 'targ_alpha_tru64',
                                 'x86-Linux'   : 'targ_ia32_ia64_linux',
                                 'x86_64-Linux': 'targ_ia64_ia64_linux',
                                 'x86-Cygwin'  : 'targ_ia32_ia64_linux',
                                 'ia64-Linux'  : 'targ_ia64_ia64_linux',
                                 'mips-IRIX64' : 'targ_mips_irix',
                                 'sparc-SunOS' : 'targ_sparc_solaris'}
    
    #Generate canonical platform
    get_platform = 'cd '+OpenADRoot+'/config && ./hpcplatform'
    p = subprocess.Popen(get_platform, shell=True,stdout=subprocess.PIPE)
    self.platform=(p.stdout.read()).rstrip()
    o64targ = platformToOpen64TargTable[self.platform]
    self.RootEnvVars = {
       'OPEN64ROOT':os.path.join(os.environ['OPEN64_BASE'],'osprey1.0',o64targ),
       'OPENADFORTTKROOT':os.path.join(os.environ['OPENADFORTTK_BASE'],'OpenADFortTk-'+self.platform),
       'OPENANALYSISROOT':os.path.join(os.environ['OPENANALYSIS_BASE'],self.platform),
       'XERCESCROOT':os.path.join(os.environ['XERCESC_BASE'],self.platform),
       'XAIFBOOSTERROOT':os.path.join(os.environ['XAIFBOOSTER_BASE'],'..'),
       'BOOSTROOT':os.environ['BOOST_BASE'],
       'ANGELROOT':os.environ['ANGEL_BASE'],
       'XAIFSCHEMAROOT':os.environ['XAIFSCHEMA_BASE'],
       'OPENADFORTTK':os.path.join(os.environ['OPENADFORTTK_BASE'],'OpenADFortTk-'+self.platform)}

    self.setPythonRootEnvVars()

    xbase=os.path.join(os.environ['XAIFBOOSTERROOT'],'xaifBooster')
    ii_xaif=os.path.join(os.environ['XAIFSCHEMAROOT'],'schema/examples/inlinable_intrinsics.xaif')
    if (self.platform=='i686-Cygwin'):
      ii_xaif = '\`cygpath -w ${ii_xaif}\`'
    self.Aliases = {
       'mfef90':os.path.join(os.environ['OPEN64ROOT'],'crayf90','sgi','mfef90'),
       'whirl2f':os.path.join(os.environ['OPEN64ROOT'],'whirl2f','whirl2f'),
       'whirl2xaif':os.path.join(os.environ['OPENADFORTTKROOT'],'bin','whirl2xaif'),
       'xaif2whirl':os.path.join(os.environ['OPENADFORTTKROOT'],'bin','xaif2whirl'),
       'whirl2sexp':os.path.join(os.environ['OPENADFORTTKROOT'],'bin','whirl2sexp'),
       'whirl2f90':os.path.join(os.environ['OPEN64ROOT'],'whirl2f','whirl2f90'),
       'ir_b2a':os.path.join(os.environ['OPEN64ROOT'],'ir_tools','ir_b2a'),
       'ir_size':os.path.join(os.environ['OPEN64ROOT'],'ir_tools','ir_size'),
       'xboostread':os.path.join(xbase,'system','test','t')+' -c '+ii_xaif,
       'xboost_l':os.path.join(xbase,'algorithms','Linearization','test','t')+' -c '+ii_xaif,
       'xboost_bb':os.path.join(xbase,'algorithms','BasicBlockPreaccumulation','test','t')+' -c '+ii_xaif,
       'xboost_bbt':os.path.join(xbase,'algorithms','BasicBlockPreaccumulationTape','test','t')+' -c '+ii_xaif,
       'xboost_bbr':os.path.join(xbase,'algorithms','BasicBlockPreaccumulationReverse','test','t')+' -c '+ii_xaif,
       'xboost_cfr':os.path.join(xbase,'algorithms','ControlFlowReversal','test','t')+' -c '+ii_xaif}


    # set OpenAD environment variables in python environment (called in __init__)
  def setPythonOpenADEnvVars(self):
    for repo in self.OpenADRepos.values():
      os.environ[repo.getVar()] = os.path.abspath(os.path.join(repo.getLocalPath(), repo.getLocalName()))
    os.environ['OPENAD_BASE'] = OpenADRoot
  
    # set Root environment variables in python environment (called in __init__)
  def setPythonRootEnvVars(self):
    for var,val in self.RootEnvVars.items():
      os.environ[var] = os.path.abspath(val)
    os.environ['OPENADROOT'] = os.environ['OPENAD_BASE']

    # set paths for python environment
  def setPaths(self):
    sys.path.append(os.path.join(os.environ['OPENADFORTTK'],'bin'))
    sys.path.append(os.path.join(os.environ['OPENADROOT'],'bin'))
    if(self.platform == 'i686-Cygwin'):
      path = os.path.join(os.environ['XERCESCROOT'],'bin:',os.environ['XERCESCROOT'],'lib:',os.environ['OPEN64ROOT'],'be:',os.environ['OPEN64ROOT'],'whirl2f:',os.environ['PATH'])
      sys.path.append(path)
    else:
      ldlib = os.path.join(os.environ['OPEN64ROOT'],'whirl2f')
      if os.getenv('LD_LIBRARY_PATH') == None:
        os.environ['LD_LIBRARY_PATH'] = ldlib
      else:
        os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH')+":"+ldlib

  def getRepos(self):
    return self.OpenADRepos
