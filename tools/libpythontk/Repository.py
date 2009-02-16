#!usr/bin/env python
import os
import sys
import re
import tempfile
from RunCmds import CmdDesc

class RepositoryException(Exception):
  def __init__(self,reason):
    Exception.__init__(self,reason)

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
  def getVersionTag(self, localRev=False):
    return self.getTag()
  def getVar(self):
    return self.var

  def getLocalRepoPath(self):
    localRepoPath = os.path.join(self.getLocalPath(),self.getLocalName())
    if self.getSubdir() is not None:
      localRepoPath = os.path.join(localRepoPath,self.getSubdir())
    return localRepoPath

  def repoExists(self):
    return os.path.exists(self.getLocalRepoPath())
  
class NoRepository(Repository):
  
  @staticmethod
  def isRepo(dir):
    return os.path.isdir(dir)

  @staticmethod
  def instanceFrom(dir):
    if (not NoRepository.isRepo(dir)):
      raise RepositoryException, dir+" is not a directory"
    (localPath,localName)=os.path.split(dir)   
    return NoRepository('n/a', localPath, localName,  None, None, None)

  def kind(self):
    return 'n/a'
  
  def writeable(self):
    return False

  def locallyModified(self):
    return False

  def incoming(self):
    return False

  def outgoing(self):
    return False
                                          
class CVSRepository(Repository):

  @staticmethod
  def isRepo(dir):
    return os.path.isdir(os.path.join(dir,'CVS'))

  @staticmethod
  def instanceFrom(dir):
    if (not CVSRepository.isRepo(dir)):
      raise RepositoryException, dir+" is not a CVS repository"   
    rootFile=open(os.path.join(dir,'CVS','Root'))
    rootString=rootFile.readline()
    rootFile.close()
    rsh=(rootString.split(':'))[1]
    url=rootString
    (localPath,localName)=os.path.split(dir)
    repoFile=open(os.path.join(dir,'CVS','Repository'))
    repoString=repoFile.readline()
    repoFile.close()
    subdir=None
    tag=None
    if os.path.isfile(os.path.join(dir,'CVS','Tag')):
      tagFile=open(os.path.join(dir,'CVS','Tag'))
      tag=tagFile.readline()[1:].strip()
      tagFile.close()
    return CVSRepository(rsh,url,localPath,localName,subdir,tag,None)

  def __init__(self,rsh,url,localPath,localName,subdir,tag,var):
    Repository.__init__(self,url,localPath,localName,subdir,tag,var)
    self.rsh=rsh
    self.env = 'CVS_RSH="' + rsh + '"'
    self.opt = '-z3 -d'

  def kind(self):
    return 'cvs'
  
  def writeable(self):
    return (re.search('anonymous', self.getUrl()) is None)

  def locallyModified(self):
    fName=tempfile.mktemp()
    os.environ['CVS_RSH']=self.getRSH()
    os.system('cd '+self.getLocalRepoPath()+'; cvs -n update 2>&1 | grep -v \'cvs update: Updating\' | grep -v \"^U\" > '+fName)
    info=os.stat(fName)
    os.remove(fName)
    return (info[6]>0)

  def incoming(self):
    fName=tempfile.mktemp()
    ret=os.system('cd '+self.getLocalRepoPath()+'; cvs -n update 2>&1 | grep -v \'cvs update: Updating\' | grep \"^U\" > '+fName)
    infoFile=open(fName)
    lines=infoFile.readlines()
    infoFile.close()    
    os.remove(fName)
    return (len(lines)>1)

  def outgoing(self):
    return self.locallyModified()

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
    if not os.path.exists(os.path.join(self.getLocalRepoPath(),'CVS')):
      raise RepositoryException("a directory "+self.getLocalRepoPath()+" exists but is not a CVS working directory")
    self.cmdDesc.setCmd("cd "+self.getLocalRepoPath()+" && "+self.env+"  cvs " + self.opt + " " + self.getUrl() + " update -d")
    self.cmdDesc.setDesc("updating "+self.getLocalRepoPath())

  def checkout(self):
    if self.getSubdir() is not None:
      name = os.path.join(self.getLocalName(),self.getSubdir())
    else:
      name=self.getLocalName()
    self.cmdDesc.setCmd(self.env+" cvs " + self.opt + " " + self.getUrl() + " co "+self.getCVSTagOpt()+" "+name)
    self.cmdDesc.setDesc("checking out into "+self.getLocalRepoPath())

class SVNRepository(Repository):

  @staticmethod
  def isRepo(dir):
    return os.path.isdir(os.path.join(dir,'.svn'))

  @staticmethod
  def instanceFrom(dir):
    if (not SVNRepository.isRepo(dir)):
      raise RepositoryException, dir+" is not an SVN repository"   
    fName=tempfile.mktemp()
    os.system('cd '+dir+'; svn info > '+fName+'; cd ../')
    infoFile=open(fName)
    infoString=''
    while 1:
        infoString=infoFile.readline()
        if (infoString[:5]=='URL: ' or len(infoString)==0):
            break
    infoFile.close()
    os.remove(fName)
    if (infoString[:5]!='URL: '): 
      raise RepositoryExceptuuib, "cannot find url for "+dir   
    url=infoString[5:]
    (localPath,localName)=os.path.split(dir)
    return SVNRepository(url,localPath,localName,None,None,None)

  def __init__(self,url,localPath, localName,subdir,tag,var):
    Repository.__init__(self,url,localPath,localName,subdir,tag,var)

  def kind(self):
    return 'svn'

  def writeable(self):
    return (re.search('http://', self.getUrl()) is None)

  def locallyModified(self):
    fName=tempfile.mktemp()
    os.system('cd '+self.getLocalRepoPath()+'; svn status | grep -vE \"^\? \" > '+fName)
    info=os.stat(fName)
    os.remove(fName)
    return (info[6]>0)

  def incoming(self):
    fName=tempfile.mktemp()
    ret=os.system('cd '+self.getLocalRepoPath()+'; svn status -uq | grep -vE \"^M \" > '+fName)
    infoFile=open(fName)
    lines=infoFile.readlines()
    infoFile.close()    
    os.remove(fName)
    return (len(lines)>1)

  def outgoing(self):
    return self.locallyModified()

  def getVersionTag(self,localRev=False):
    fName=tempfile.mktemp()
    ret=os.system('cd '+self.getLocalRepoPath()+'; svn info | grep Revision: | sed \'s/Revision: //\' > '+fName)
    infoFile=open(fName)
    lines=infoFile.readlines()
    infoFile.close()    
    os.remove(fName)
    return lines[0].strip()

  def update(self):
    if not os.path.exists(os.path.join(self.getLocalRepoPath(),'.svn')):
      raise RepositoryException("a directory "+self.getLocalRepoPath()+" exists but is not a SVN working directory")
    self.cmdDesc.setCmd("cd "+self.getLocalRepoPath()+" && svn update")
    self.cmdDesc.setDesc("updating "+self.getLocalRepoPath())

  def checkout(self):
    url=self.getUrl()
    if self.getTag() is not None:
      url+=self.getTag()
    if self.getSubdir() is not None:
      name = os.path.join(self.getLocalName(),self.getSubdir())
      url=os.path.join(url,self.getSubdir())
    else:
      name=self.getLocalName()
    self.cmdDesc.setCmd("svn co "+url+" "+name)
    self.cmdDesc.setDesc("checking out into "+self.getLocalRepoPath())

class MercurialRepository(Repository):

  @staticmethod
  def isRepo(dir):
    return os.path.isdir(os.path.join(dir,'.hg'))

  @staticmethod
  def instanceFrom(dir):
    if (not MercurialRepository.isRepo(dir)):
      raise RepositoryException, dir+" is not a Mercurial repository"   
    fName=tempfile.mktemp()
    os.system('cd '+dir+'; hg show > '+fName+'; cd ../')
    infoFile=open(fName)
    infoString=''
    headAttribute='paths.default='
    while 1:
        infoString=infoFile.readline()
        if (infoString[:len(headAttribute)]==headAttribute or len(infoString)==0):
            break
    infoFile.close()
    os.remove(fName)
    if (infoString[:len(headAttribute)]!=headAttribute):
        raise RepositoryException, "cannot find url for "+rep   
    url=infoString[len(headAttribute):]
    (localPath,localName)=os.path.split(dir)
    return MercurialRepository(url,localPath,localName,None,None,None)

  def __init__(self,url,localPath, localName,subdir,tag,var):
    Repository.__init__(self,url,localPath,localName,subdir,tag,var)

  def kind(self):
    return 'hg'

  def writeable(self):
    return (re.search('http://', self.getUrl()) is None)

  def locallyModified(self):
    # local changes
    fName=tempfile.mktemp()
    os.system('cd '+self.getLocalRepoPath()+'; hg status > '+fName)
    changes=(os.stat(fName)[6]>0)
    os.remove(fName)
    # tip
    fName=tempfile.mktemp()
    os.system('cd '+self.getLocalRepoPath()+'; hg  tip  --template \'{node|short}\\n\' > '+fName)
    infoFile=open(fName)
    tip=infoFile.readlines()[0].strip()
    infoFile.close()
    os.remove(fName)
    id=self.getVersionTag()
    return (changes or tip != id)

  def incoming(self):
    fName=tempfile.mktemp()
    ret=os.system('cd '+self.getLocalRepoPath()+'; hg incoming -q > '+fName)
    info=os.stat(fName)
    os.remove(fName)
    return (info[6]>0)

  def outgoing(self):
    fName=tempfile.mktemp()
    ret=os.system('cd '+self.getLocalRepoPath()+'; hg outgoing -q > '+fName)
    info=os.stat(fName)
    os.remove(fName)
    return (info[6]>0)

  def getVersionTag(self,localRev=False):
    versionTag=''
    fName=tempfile.mktemp()
    if localRev:
      os.system('cd '+self.getLocalRepoPath()+'; hg id -n > '+fName)
      infoFile=open(fName)
      lines=infoFile.readlines()
      infoFile.close()
      versionTag=lines[0].strip()+":"
    os.system('cd '+self.getLocalRepoPath()+'; hg id -i > '+fName)
    infoFile=open(fName)
    lines=infoFile.readlines()
    infoFile.close()    
    os.remove(fName)
    return versionTag+lines[0].strip()

  def update(self):
    if not os.path.exists(os.path.join(self.getLocalRepoPath(),'.hg')):
      raise RepositoryException("a directory "+self.getLocalRepoPath()+" exists but is not a Mercurial working directory")
    self.cmdDesc.setCmd("cd "+self.getLocalRepoPath()+" && hg pull -q && hg update")
    self.cmdDesc.setDesc("updating "+self.getLocalRepoPath())

  def checkout(self):
    cmd=""
    desc="cloning into "
    if self.getSubdir() is not None:
      raise RepositoryException("For a Mercurial repository one cannot specify a subdirectory to be cloned")
    if self.getLocalPath() is not None:
      cmd="cd "+self.getLocalPath()+" && "
      desc+=os.path.join(self.getLocalPath(),self.getLocalName())
    else:
      desc+=self.getLocalName()
    cmd+="hg clone "+self.getUrl()+" "+self.getLocalRepoPath()
    self.cmdDesc.setCmd(cmd)
    self.cmdDesc.setDesc(desc)


class Detect:

  @staticmethod
  def makeRepo(dir):
    # figure out what it is
    matches=0
    repo=None
    if (CVSRepository.isRepo(dir)):
        matches+=1
        repo=CVSRepository.instanceFrom(dir)
    if (SVNRepository.isRepo(dir)):
        matches+=1
        repo=SVNRepository.instanceFrom(dir)
    if (MercurialRepository.isRepo(dir)):
        matches+=1
        repo=MercurialRepository.instanceFrom(dir)
    if (matches>1):
        raise RepositoryException, "more than one possible repository type for "+dir   
    if (matches<1):
        if (NoRepository.isRepo(dir)):
           repo=NoRepository.instanceFrom(dir)
        else : 
           raise RepositoryException, "cannot determine type for "+dir
    return repo

    
