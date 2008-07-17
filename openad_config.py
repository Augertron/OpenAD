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
libpythontk = os.path.join(OpenADRoot,'tools','libpythontk')
sys.path.append(libpythontk)
import RunCmds
import Repository
import RepositoryTools

######################################################################
# Repository information
######################################################################

# Rice HiPerSoft SVN Repository
OPENAD_REPO_RICESVN_ROOT = 'http://hpc.svn.rice.edu/r/'

# SourceForge CVS Repositories
OPENAD_REPO_SF_ANGEL_rsh = 'pserver'
OPENAD_REPO_SF_ANGEL_Root = ':pserver:anonymous@angellib.cvs.sourceforge.net:/cvsroot/angellib'
OPENAD_REPO_SF_BOOST_rsh = 'pserver'
OPENAD_REPO_SF_BOOST_Root = ':pserver:anonymous@boost.cvs.sourceforge.net:/cvsroot/boost'

class openad_config:
  ''' A list of all repositories in this configuation of OpenAD '''
  def __init__(self):
    platformToOpen64TargTable = {'alpha-OSFI': 'targ_alpha_tru64',
                                 'x86-Linux': 'targ_ia32_ia64_linux',
                                 'x86_64-Linux': 'targ_ia64_ia64_linux',
                                 'x86-Cygwin': 'targ_ia32_ia64_linux',
                                 'ia64-Linux': 'targ_ia64_ia64_linux',
                                 'mips-IRIX64': 'targ_mips_irix',
                                 'sparc-SunOS': 'targ_sparc_solaris'}
    
    #Generate canonical platform
    get_platform = 'cd '+OpenADRoot+'/config && ./hpcplatform'
    p = subprocess.Popen(get_platform, shell=True,stdout=subprocess.PIPE)
    self.platform=(p.stdout.read()).rstrip()
    o64targ = platformToOpen64TargTable[self.platform]

    self.OPENAD_OPEN64 = Repository.SVNRepository()
    self.OPENAD_OPENADFORTTK = Repository.SVNRepository()
    self.OPENAD_OPENANALYSIS = Repository.SVNRepository()
    self.OPENAD_XERCESC = Repository.SVNRepository()
    self.OPENAD_XAIFBOOSTER = Repository.SVNRepository()
    self.OPENAD_ANGEL = Repository.CVSRepository()
    self.OPENAD_XAIF = Repository.SVNRepository()
    self.OPENAD_BOOST = Repository.CVSRepository()
    self.OpenADRepos = {"OPENAD_OPEN64":self.OPENAD_OPEN64,
                        "OPENAD_OPENADFORTTK":self.OPENAD_OPENADFORTTK,
                        "OPENAD_OPENANALYSIS":self.OPENAD_OPENANALYSIS,
                        "OPENAD_XERCESC":self.OPENAD_XERCESC,
                        "OPENAD_XAIFBOOSTER":self.OPENAD_XAIFBOOSTER,
                        "OPENAD_ANGEL":self.OPENAD_ANGEL,
                        "OPENAD_XAIF":self.OPENAD_XAIF,
                        "OPENAD_BOOST":self.OPENAD_BOOST}
######################################################################
# set OpenAD Repositories
###################################################################### 

    self.OPENAD_OPEN64.setAll('Open64',OpenADRoot,None,"OpenAD","OPEN64_BASE",OPENAD_REPO_RICESVN_ROOT+'open64/trunk/')
    self.OPENAD_OPENADFORTTK.setAll('OpenADFortTk',OpenADRoot,None,None,'OPENADFORTTK_BASE',OPENAD_REPO_RICESVN_ROOT+'OpenADFortTk/trunk')
    self.OPENAD_OPENANALYSIS.setAll('OpenAnalysis',OpenADRoot,None,None,'OPENANALYSIS_BASE',OPENAD_REPO_RICESVN_ROOT+'open-analysis/tags/version-openad')
    self.OPENAD_XERCESC.setAll('xercesc', OpenADRoot,None,None,'XERCESC_BASE',OPENAD_REPO_RICESVN_ROOT+'xercesc/tags/version-openad')
    self.OPENAD_XAIFBOOSTER.setAll('xaifBooster',OpenADRoot,None,None,'XAIFBOOSTER_BASE',OPENAD_REPO_RICESVN_ROOT+'xaifBooster/trunk')
    self.OPENAD_ANGEL.setAll('angel',OpenADRoot,None,None,'ANGEL_BASE',OPENAD_REPO_SF_ANGEL_rsh,OPENAD_REPO_SF_ANGEL_Root)
    self.OPENAD_XAIF.setAll('xaif',OpenADRoot,None,None,'XAIFSCHEMA_BASE',OPENAD_REPO_RICESVN_ROOT+'xaif/trunk')
    self.OPENAD_BOOST.setAll('boost/',OpenADRoot,"boost",'Version_1_34_1','BOOST_BASE',OPENAD_REPO_SF_BOOST_rsh,OPENAD_REPO_SF_BOOST_Root)
######################################################################
# set OpenAD Environment Variables
######################################################################
    self.setPythonOpenADEnvVars()
    
######################################################################
# define RootEnvVars
######################################################################

    self.RootEnvVars = {'OPEN64ROOT':os.path.join(os.environ['OPEN64_BASE'],'osprey1.0',o64targ),
       'OPENADFORTTKROOT':os.path.join(os.environ['OPENADFORTTK_BASE'],'OpenADFortTk-'+self.platform),
       'OPENANALYSISROOT':os.path.join(os.environ['OPENANALYSIS_BASE'],self.platform),
       'XERCESCROOT':os.path.join(os.environ['XERCESC_BASE'],self.platform),
       'XAIFBOOSTERROOT':os.path.join(os.environ['XAIFBOOSTER_BASE'],'..'),
       'BOOSTROOT':os.environ['BOOST_BASE'],
       'ANGELROOT':os.environ['ANGEL_BASE'],
       'XAIFSCHEMAROOT':os.environ['XAIFSCHEMA_BASE'],
       'OPENADFORTTK':os.path.join(os.environ['OPENADFORTTK_BASE'],'OpenADFortTk-'+self.platform)}

######################################################################
# set RootEnvVars
######################################################################
    self.setPythonRootEnvVars()

######################################################################
# define Aliases
######################################################################
    xbase=os.path.join(os.environ['XAIFBOOSTERROOT'],'xaifBooster')
    ii_xaif=os.path.join(os.environ['XAIFSCHEMAROOT'],'schema/examples/inlinable_intrinsics.xaif')
    if (self.platform=='i686-Cygwin'):
      ii_xaif = '\`cygpath -w ${ii_xaif}\`'
    self.Aliases = {
       'mfef90':os.path.join(os.environ['OPEN64ROOT'],'crayf90/sgi/mfef90'),
       'whirl2f':os.path.join(os.environ['OPEN64ROOT'],'whirl2f/whirl2f'),
       'whirl2f90':os.path.join(os.environ['OPEN64ROOT'],'whirl2f/whirl2f90'),
       'ir_b2a':os.path.join(os.environ['OPEN64ROOT'],'ir_tools/ir_b2a'),
       'ir_size':os.path.join(os.environ['OPEN64ROOT'],'ir_tools/ir_size'),
       'xboostread':xbase+'/system/test/t -c '+ii_xaif,
       'xboost_l':xbase+'/algorithms/Linearization/test/t -c '+ii_xaif,
       'xboost_bb':xbase+'/algorithms/BasicBlockPreaccumulation/test/t -c '+ii_xaif,
       'xboost_bbt':xbase+'/algorithms/BasicBlockPreaccumulationTape/test/t -c '+ii_xaif,
       'xboost_bbr':xbase+'/algorithms/BasicBlockPreaccumulationReverse/test/t -c '+ii_xaif,
       'xboost_cfr':xbase+'/algorithms/ControlFlowReversal/test/t -c '+ii_xaif}


# set OpenAD environment variables in python environment (called in __init__)
  def setPythonOpenADEnvVars(self):
    for repo in self.OpenADRepos.values():
      os.environ[repo.getVar()] = os.path.abspath(os.path.join(repo.getComponentPath(), repo.getName()))
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

# getRepos: returns an array of 'RepositoryDesc' references
#   containing the repository information
  def getRepos(self):
    return self.OpenADRepos
