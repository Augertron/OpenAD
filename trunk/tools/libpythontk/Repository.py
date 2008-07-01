#!usr/bin/env python
import os
import sys
import RunCmds
from RunCmds import CmdDesc

class Repository:

  def __init__(self):
    self.name = None   # name
    self.componentPath=None   # path to repository component
    self.subdir=None    # subdirectory of repository (optional)
    self.tag=None       # interpreted as a tag/branch by default;
    self.var=None       # corresponding environment variable
    self.cmdDesc=CmdDesc() # command description for updates

  def setName(self, name):
    self.name = name
  def setComponentPath(self, path):
    self.path = path
  def setSubdir(self, subdir):
    self.subdir=subdir
  def setTag(self, tag):
    self.tag=tag
  def setVar(self, var):
    self.var=var
  def setAll(self, name, path, subdir, tag, var):
    self.setName(name)
    self.setComponentPath(path)
    self.setSubdir(subdir)
    self.setTag(tag)
    self.setVar(var)

  def getName(self):
    return self.name
  def getComponentPath(self):
    return self.path
  def getSubdir(self):
    return self.subdir
  def getTag(self):
    return self.tag
  def getVar(self):
    return self.var


  def repoExists(self):
    localRepoPath = os.path.join(self.getComponentPath(),self.getName())
    if self.getSubdir() is not None:
      localRepoPath = os.path.join(localRepoPath,self.getSubdir())
    return os.path.exists(localRepoPath)
  
class CVSRepository(Repository):

  def __init__(self):
    Repository.__init__(self)
    self.rsh = None
    self.root=None

  def setRSH(self, rsh):
    self.rsh = rsh
    self.env = 'CVS_RSH="' + self.getRSH() + '"'
  def setRoot(self, root):
    self.root = root
    self.opt = '-z3 -d' + self.getRoot()
  def setAll(self,name,path,subdir,tag,var,rsh, root):
    Repository.setAll(self,name,path,subdir,tag,var)
    self.setRSH(rsh)
    self.setRoot(root)


  def getRSH(self):
    return self.rsh
  def getRoot(self):
    return self.root
  
# getCVSTagOpt: 
  def getCVSTagOpt(self):
    opt=""
    if self.getTag():
      date = ""
      re = '^{date}(.*)'
      if ((date) == self.getTag().find('/'+re+'/')): 
        opt = '-D '+date
      else:
        opt = "-r "+self.getTag()
    return opt


# set command description to update repository
  def update(self):
    localRepoPath = os.path.join(self.getComponentPath(),self.getName())
    self.cmdDesc.setCmd("cd "+localRepoPath+" && "+self.env+"  cvs "+self.opt+" update -d")
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())

# set command to check out repository
  def checkout(self):
    topt = self.getCVSTagOpt()
    self.cmdDesc.setCmd(self.env+" cvs "+self.opt+" co "+topt+" "+self.getName())
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())

# set command description to check out subdirectory
  def checkoutSubdir(self):
    nm = os.path.join(self.getName(),self.getSubdir())
    topt = self.getCVSTagOpt()
    self.cmdDesc.setCmd(self.env+" cvs "+self.opt+" co "+topt+" "+nm)
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())

class SVNRepository(Repository):

  def __init__(self):
    Repository.__init__(self)
    self.root=None #root directory for repositories

  def setRoot(self, root):
    self.root = root
  def setAll(self,name,path,subdir,tag,var, root):
    Repository.setAll(self,name,path,subdir,tag,var)
    self.setRoot(root)

  def getRoot(self):
    return self.root

# set command description to update repository
  def update(self):
    localRepoPath = os.path.join(self.getComponentPath(),self.getName())
    self.cmdDesc.setCmd("cd "+localRepoPath+" && svn update")
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())

# set command description to checkout repository
  def checkout(self):
    self.cmdDesc.setCmd("svn co "+self.getRoot()+" "+self.getName())
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())

  def checkoutSubdir(self):
    name = os.path.join(self.getName(),self.getSubdir())
    self.cmdDesc.setCmd("svn co "+self.getRoot()+" "+name)
    self.cmdDesc.setDesc(self.cmdDesc.getCmd())
