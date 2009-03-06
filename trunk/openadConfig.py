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

class openadConfig:
  ''' A list of all repositories in this configuation of OpenAD '''
  def __init__(self, includeTests=False, includeExtras=False, includeDev=False):
    self.orderedRepoList=[] # we need something that is explicitly ordered
    riceSvnUrl = 'http://hpc.svn.rice.edu/r/'
    self.OpenADRepos = {}
    name="OpenAD";        self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+name,OpenADRoot,'.',None,'trunk',None)
    name="Open64";        self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+'open64',OpenADRoot,name,None,'tags/version-openad',"OPEN64_BASE")
    name="OpenADFortTk";  self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+name,OpenADRoot,name,None,'trunk','OPENADFORTTK_BASE')  
    name="OpenAnalysis";  self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+'open-analysis',OpenADRoot,name,None,'tags/version-openad','OPENANALYSIS_BASE')
    name="xercesc";       self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+name,OpenADRoot,name,None,'tags/version-openad','XERCESC_BASE')
    name="xaifBooster";   self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+name,OpenADRoot,name,None,'trunk','XAIFBOOSTER_BASE')
    name="xaif";          self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository(riceSvnUrl+name,OpenADRoot,name,None,'trunk','XAIFSCHEMA_BASE')
    name="angel";         self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.CVSRepository('pserver',':pserver:anonymous@angellib.cvs.sourceforge.net:/cvsroot/angellib',OpenADRoot,name,None,None,'ANGEL_BASE')
    name="boost";         self.orderedRepoList.append(name)
    self.OpenADRepos[name]=Repository.SVNRepository('http://svn.boost.org/svn/boost',OpenADRoot,name,'boost','tags/release/Boost_1_38_0','BOOST_BASE')
    ANLMercurialUrl = 'http://mercurial.mcs.anl.gov//ad/'
    if includeTests:
      name="RegressionOpenAD"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+name,OpenADRoot,'Regression',None,None,None)
      name="RegressionOpenADFortTk"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+name,os.path.join(OpenADRoot,'OpenADFortTk'),'Regression',None,None,None)
      name="RegressionSourceProcessing"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+name,os.path.join(OpenADRoot,'OpenADFortTk','tools','SourceProcessing'),'Regression',None,None,None)
    if includeExtras:
      name="RevolveF9X"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+name,OpenADRoot,name,None,None,None)
      name="Examples"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+'OpenADExamples',OpenADRoot,name,None,None,None)
    if includeDev:
      name="SourceProcessing"; self.orderedRepoList.append(name)
      self.OpenADRepos[name]=Repository.MercurialRepository(ANLMercurialUrl+name,os.path.join(OpenADRoot,'OpenADFortTk','tools'),name,None,None,None)

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
       'REVOLVEF9XROOT':os.path.join(OpenADRoot,'RevolveF9X'),
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