#!/usr/bin/env python
##########################################################
# This file is part of OpenAD released under the LGPL.   #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the OpenAD distribution             #
##########################################################

import os
import sys


class RunnerException(Exception):
  def __init__(self,reason):
    Exception.__init__(self,reason)

class CmdDesc:
  def __init__(self):
    self.cmd = "undef"
    self.desc = "undef"

  def getCmd(self):
    return self.cmd
  def getDesc(self):
    return self.desc
  def setCmd(self, cmd):
    self.cmd = cmd
  def setDesc(self,desc):
    self.desc = desc

class Runner:
  def __init__(self,verbose, interactive, logfnm, keepGoing):
    self.verbose=verbose
    self.interactive=interactive
    self.logfnm=logfnm
    self.keepGoing=keepGoing

  def doit(self, cmdDescVecRef):
    for desc in cmdDescVecRef:
      if self.interactive:
         if (raw_input("Execute command: \"%s\" ? (y)/n: " % (desc.getCmd())) == "n"):
           sys.stdout.flush()
           continue
      try:  
        self.__doSingle(desc.getCmd(), desc.getDesc())
      except RunnerException, e:
        if self.keepGoing:
          sys.stderr.write('ERROR: '+str(e)+'... continuing\n')
        else:
          raise e

  def __doSingle(self,cmd,desc):
    dir = os.getcwd()
    outfnm = os.path.join(dir,"RunCmd-out.tmp~")
    f = open(outfnm,"w")
    f.close()
    cmd += " > "+outfnm+" 2>&1"
    if self.logfnm:
      # Append separator and command to log file
      try:
        logfh = open(self.logfnm,"a")
        logfh.write("*****************************************************************************\n\n")
        logfh.write(cmd+"\n")
        logfh.close()
      except IOError, e:
        print e
        sys.stderr.write("Unable to open logfh\n")
    if self.verbose >= 1:
      sys.stdout.write(desc+"\n")
    if self.verbose >= 2:
      sys.stdout.write("Executing "+cmd+"\n")
    try:
      ret=os.system(cmd)
      if ret!=0 : 
	raise RunnerException(cmd+" return value "+str(ret))
    except Exception, e:
      self.__appendOutfileToLogfile(outfnm)
      os.system("cat "+outfnm)
      raise e
    self.__appendOutfileToLogfile(outfnm)
    os.remove(outfnm)

  def __appendOutfileToLogfile(self, outfnm):
    os.system("cat "+outfnm+" >> "+self.logfnm)
