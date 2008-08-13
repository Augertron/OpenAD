#!/usr/bin/env python

import os
import sys


class RunCmdsException(Exception):
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



class RunCmds:
  def __init__(self):
    None

  def RunCmds(self, cmdDescVecRef, verbose, interactive, logfnm):
    for desc in cmdDescVecRef:
      if interactive >= 1:
        sys.stdout.write("Execute? "+desc.getCmd()+"\n")
        ans =""
        while (ans != "Y") and (ans != "y") and (ans != "N") and (ans !="n"):
          sys.stdout.write("...[Y/n]\n")
          ans = sys.stdin.read(1)
        if ans == 'n' or ans == 'N':
          break
      self.RunCmd(desc.getCmd(), desc.getDesc(), verbose, logfnm)
  
  def RunCmd(self, cmd, desc, verbose, logfnm):
    dir = os.getcwd()
    outfnm = os.path.join(dir,"RunCmd-out.tmp~")
    f = open(outfnm,"w")
    f.close()
    cmd += " > "+outfnm+" 2>&1"
    if logfnm:
      # Append separator and command to log file
      try:
        logfh = open(logfnm,"a")
        logfh.write("*****************************************************************************\n\n")
        logfh.write(cmd+"\n")
        logfh.close()
      except IOError, e:
        print e
        sys.stderr.write("Unable to open logfh\n")

    if verbose >= 1:
      sys.stdout.write(desc+"\n")

    if verbose >= 2:
      sys.stdout.write("Executing "+cmd+"\n")

    try:
      ret=os.system(cmd)
      if ret!=0 : 
	raise RunCmdsException(cmd+" return value "+str(ret))
    except Exception, e:
      self.AppendOutfileToLogfile(logfnm, outfnm)
      os.system("cat "+outfnm)
      raise e

    self.AppendOutfileToLogfile(logfnm, outfnm)
    os.remove(outfnm)

  def AppendOutfileToLogfile(self,logfnm, outfnm):
    os.system("cat "+outfnm+" >> "+logfnm)
