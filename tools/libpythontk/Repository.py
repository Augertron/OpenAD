#!usr/bin/env python
import os
import sys
from RunCmds import CmdDesc

class Repository:

  def __init__(self,url, localPath, localName,  subdir, tag, var):
    self.url=url        # the url of the repository
    self.localPath=localPath # absolute path to the local directory in which the working directory 'localName' resides 
    self.localName=localName # the local name
    self.subdir=subdir  # subdirectory of repository (optional, used to speed up checkouts/updates)
    self.tag=tag        # interpreted as a tag/branch by default;
    self.var=var        # corresponding environment variable
    self.cmdDesc=CmdDesc() # command description for updates

  def getUrl(self):
    return self.url
  def getLocalName(self):
    return self.localName
  def getLocalPath(self):
    return self.localPath
  def getSubdir(self):
    return self.subdir
  def getTag(self):
    return self.tag
  def getVar(self):
    return self.var

  def getLocalRepoPath(self):
    localRepoPath = os.path.join(self.getLocalPath(),self.getLocalName())
    if self.getSubdir() is not None:
      localRepoPath = os.path.join(localRepoPath,self.getSubdir())
    return localRepoPath

  def repoExists(self):
    return os.path.exists(self.getLocalRepoPath())
  
class CVSRepository(Repository):

  def __init__(self,rsh,url,localPath,localName,subdir,tag,var):
    Repository.__init__(self,url,localPath,localName,subdir,tag,var)
    self.env = 'CVS_RSH="' + rsh + '"'
    self.opt = '-z3 -d'

  def getRSH(self):
    return self.rsh
  
  # getCVSTagOpt: 
  def getCVSTagOpt(self):
    tagOpts=""
    if self.getTag():
      date = ""
      re = '^{date}(.*)'
      if ((date) == self.getTag().find('/'+re+'/')): 
        tagOpts = '-D '+date
      else:
        tagOpts = "-r "+self.getTag()
    return tagOpts
  
  def update(self):
    self.cmdDesc.setCmd("cd "+self.getLocalRepoPath()+" && "+self.env+"  cvs " + self.opt + " " + self.getUrl() + " update -d")
    self.cmdDesc.setDesc("updating "+self.getLocalName())

  def checkout(self):
    if self.getSubdir() is not None:
      name = os.path.join(self.getLocalName(),self.getSubdir())
    else:
      name=self.getLocalName()
    self.cmdDesc.setCmd(self.env+" cvs " + self.opt + " " + self.getUrl() + " co "+self.getCVSTagOpt()+" "+name)
    self.cmdDesc.setDesc("checking out "+self.getLocalName())

class SVNRepository(Repository):

  def __init__(self,url,localPath, localName,subdir,tag,var):
    Repository.__init__(self,url,localPath,localName,subdir,tag,var)

  def update(self):
    self.cmdDesc.setCmd("cd "+self.getLocalRepoPath()+" && svn update")
    self.cmdDesc.setDesc("updating "+self.getLocalName())

  def checkout(self):
    if self.getSubdir() is not None:
      name = os.path.join(self.getLocalName(),self.getSubdir())
    else:
      name=self.getLocalName()
    self.cmdDesc.setCmd("svn co "+self.getUrl()+" "+name)
    self.cmdDesc.setDesc("checking out "+self.getLocalName())

