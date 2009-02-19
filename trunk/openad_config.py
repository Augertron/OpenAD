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
  def __init__(self, includeTests=False):
    self.orderedRepoList=[] # we need something that is explicitly ordered
    riceSvnUrl = 'http://hpc.svn.rice.edu/r/'
    self.skeletonRepo=Repository.SVNRepository(riceSvnUrl+'OpenAD/trunk',OpenADRoot,'.',None,None,None)
    self.orderedRepoList.append('skeletonRepo')
    self.OpenADRepos = {}
    # Open64
    self.OpenADRepos["Open64"]=Repository.SVNRepository(riceSvnUrl+'open64',OpenADRoot,'Open64',None,'tags/version-openad',"OPEN64_BASE")
    self.orderedRepoList.append("Open64")
    # OpenADFortTk
    self.OpenADRepos["OpenAdFortTk"]=Repository.SVNRepository(riceSvnUrl+'OpenADFortTk',OpenADRoot,'OpenADFortTk',None,'trunk','OPENADFORTTK_BASE')  
    self.orderedRepoList.append("OpenAdFortTk")
    # OpenAnalysis
    self.OpenADRepos["OpenAnalysis"]=Repository.SVNRepository(riceSvnUrl+'open-analysis',OpenADRoot,'OpenAnalysis',None,'tags/version-openad','OPENANALYSIS_BASE')
    self.orderedRepoList.append("OpenAnalysis")
    # xercesc
    self.OpenADRepos["xercesc"]=Repository.SVNRepository(riceSvnUrl+'xercesc',OpenADRoot,'xercesc', None, 'tags/version-openad','XERCESC_BASE')
    self.orderedRepoList.append("xercesc")
    # xaifBooster
    self.OpenADRepos["xaifBooster"]=Repository.SVNRepository(riceSvnUrl+'xaifBooster',OpenADRoot,'xaifBooster',None,'trunk','XAIFBOOSTER_BASE')
    self.orderedRepoList.append("xaifBooster")
    # xaif
    self.OpenADRepos["xaif"]=Repository.SVNRepository(riceSvnUrl+'xaif',OpenADRoot,'xaif',None,'trunk','XAIFSCHEMA_BASE')
    self.orderedRepoList.append("xaif")
    # angel
    self.OpenADRepos["angel"]=Repository.CVSRepository('pserver',':pserver:anonymous@angellib.cvs.sourceforge.net:/cvsroot/angellib',OpenADRoot,'angel',None,None,'ANGEL_BASE')
    self.orderedRepoList.append("angel")
    # boost
    self.OpenADRepos["boost"]=Repository.SVNRepository('http://svn.boost.org/svn/boost',OpenADRoot,'boost','boost','tags/release/Boost_1_38_0','BOOST_BASE')
    self.orderedRepoList.append("boost")
    if includeTests:
      ANLMercurialUrl = 'http://mercurial.mcs.anl.gov//ad/'
      self.OpenADRepos["RegressionOpenAD"]=Repository.MercurialRepository(ANLMercurialUrl+'RegressionOpenAD',OpenADRoot,'Regression',None,None,None)
      self.orderedRepoList.append("RegressionOpenAD")
      self.OpenADRepos["RegressionOpenADFortTk"]=Repository.MercurialRepository(ANLMercurialUrl+'RegressionOpenADFortTk',os.path.join(OpenADRoot,'OpenADFortTk'),'Regression',None,None,None)
      self.orderedRepoList.append("RegressionOpenADFortTk")
      self.OpenADRepos["RegressionSourceProcessing"]=Repository.MercurialRepository(ANLMercurialUrl+'RegressionSourceProcessing',os.path.join(OpenADRoot,'OpenADFortTk','tools','SourceProcessing'),'Regression',None,None,None)
      self.orderedRepoList.append("RegressionSourceProcessing")
      self.OpenADRepos["Examples"]=Repository.MercurialRepository(ANLMercurialUrl+'OpenADExamples',OpenADRoot,'Examples',None,None,None)
      self.orderedRepoList.append("Examples")

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
       'ir_size':os.path.join(os.environ['OPEN64ROOT'],'ir_tools','ir_size')}


    # set OpenAD environment variables in python environment (called in __init__)
  def setPythonOpenADEnvVars(self):
    for repo in self.OpenADRepos.values():
      if repo.getVar() is not None:
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
