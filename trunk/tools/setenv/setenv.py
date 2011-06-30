#!/usr/bin/env python
##########################################################
# This file is part of OpenAD released under the LGPL.   #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the OpenAD distribution             #
##########################################################

###############################################################################
import os
import sys
import warnings
import subprocess
import getopt
# Assumes executing script is in main OpenAD directory
mydir = os.path.dirname(__file__)
OpenADRoot = os.path.abspath(os.path.join(mydir, "../.."))
sys.path.append(OpenADRoot)
import openadConfig
libsetenvdir = os.path.join(OpenADRoot, "tools/libpythontk")
sys.path.append(libsetenvdir)
import libsetenv

class setenv:
  def __init__(self):

###############################################################################
    self.the_program = os.path.basename(sys.argv[0])
    self.the_usage='usage: '+self.the_program+' [options] <shell> \
        Required arguments: \
        -s, --shell=<shell-type> : sh | csh\
    \
      General options:\
        -h, --help : print help\
      \n'

    self.the_options = 'shell:s=s,help:h'
    self.opt_shell = 'undef'
    self.help ="Try "+self.the_program+" --help' for more information.\n"


###############################################################################
# parse the command line
###############################################################################

    self.parseCmdLine(sys.argv) #assigns self.opt_shell, or exits on error
    self.libsetenv_instance = libsetenv.libsetenv(self.opt_shell)

###############################################################################
# GenEnvSettings
###############################################################################

# GenEnvSettings: Generate code to setup CVS repository variables
  def GenEnvSettings(self):
    config=openadConfig.openadConfig()
    OpenADRepos = config.getRepos()
    # BASE environment variables
    print self.libsetenv_instance.genSetEnvVar('OPENAD_BASE',OpenADRoot)
    for key,repoPair in (OpenADRepos.items()):
      if repoPair[0].getVar() is not None:
        print self.libsetenv_instance.genSetEnvVar(repoPair[0].getVar(),os.path.join(repoPair[0].getLocalPath(),repoPair[0].getLocalName()))
    # ROOT environment variables
    print self.libsetenv_instance.genSetEnvVar('OPENADROOT', OpenADRoot)
    for var,val in config.RootEnvVars.items():
      try:
        print self.libsetenv_instance.genSetEnvVar(var,val)
      except NameError, e:
        print e
        pass
    # PATH and LD_LIBRARY_PATH
    print self.libsetenv_instance.genAppendEnvVar('PATH',os.path.join(os.environ['OPENADFORTTK']+'bin'))
    print self.libsetenv_instance.genAppendEnvVar('PATH',os.path.join(os.environ['OPENADROOT'],'bin'))
    if(config.platform=='i686-Cygwin' or config.platform=='x86-Cygwin'):
      path = os.environ['XERCESCROOT']+'/bin:'+os.environ['XERCESCROOT']+'/lib:'+os.environ['OPEN64ROOT']+'/be:'+os.environ['OPEN64ROOT']+'/whirl2f:'+os.environ['PATH']
      print self.libsetenv_instance.genAppendEnvVar('PATH', path)
    else:
      ldlib=os.environ['OPEN64ROOT']+'/whirl2f'
      ldPathName='LD_LIBRARY_PATH'
      if (config.platform=='x86-MacOS'):
        ldPathName='DY'+ldPathName
      print self.libsetenv_instance.genAppendEnvVar(ldPathName, ldlib)
    # aliases
    for var,val in config.Aliases.items():
      try:
        command = self.libsetenv_instance.genSetAlias(var, val)
        print command
      except NameError, e:
        print e
        pass

# parseCmdLine: process args and assign self.opt_shell variable, or print error message
  def parseCmdLine(self, command):
    # Get optional arguments
    opts=[]
    opts,ret = getopt.getopt(command[1:],'s:h',['shell=','help'])
    ## the getopt call  removes options from ARGV and 
    ## there should be nothing left in ret.
    if len(ret) != 0:
      self.printErrorAndExit()
    # Get optional arguments: help
      try:
        self.printUsageAndExit()
      except RuntimeError, e:
        print e
        pass

  # ----------------------------------------------------------
  # Required arguments
  # ----------------------------------------------------------
  # Shell type
    for opt in opts:
      arg, self.opt_shell = opt
      if arg == "--shell":
        break
    if self.opt_shell == 'undef':
      self.printErrorAndExit("Shell option missing\n")
    testShell = libsetenv.libsetenv(self.opt_shell)
    if not (testShell.is_sh() or testShell.is_csh()):
       self.printErrorAndExit("Shell "+self.opt_shell+" is not a sh or csh type shell.\n")



  def printUsageAndExit(self):
    print self.the_usage
    sys.exit()

  def printErrorAndExit(self,msg=help):
    print msg
    sys.exit()


# ----------------------------------------------------------
# Run
# ----------------------------------------------------------

setenv=setenv()
setenv.GenEnvSettings()
