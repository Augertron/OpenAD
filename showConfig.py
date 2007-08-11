#!/usr/bin/python

import os
import sys
import re
import tempfile
from optparse import OptionParser

def repKind(rep):
    matches=0
    returnVal=''
    if (os.path.isdir('./'+rep+'/CVS')):
        matches+=1
        returnVal='cvs'
    if (os.path.isdir('./'+rep+'/.hg')):   
        matches+=1
        returnVal='hg'
    if (os.path.isdir('./'+rep+'/.svn')):   
        matches+=1
        returnVal='svn'
    if (matches>1):
        raise RuntimeError, "more than one possible repository type for "+rep   
    if (matches<1):
        raise RuntimeError, "cannot find repository type for "+rep   
    return returnVal

def cvsParent(rep):
    rootFile=open('CVS/Root')
    rootString=rootFile.readline()
    rootFile.close()
    return rootString

def svnParent(rep):
    fName=tempfile.mktemp()
    system('svn info > '+fName)
    infoFile=open(fName)
    infoString=''
    while 1:
        infoString=infoFile.readline()
        if (infoString[:5]=='URL: ' or len(infoString)==0):
            break
    infoFile.close()
    if (infoString[:5]!='URL: '): 
        raise RuntimeError, "cannot find url for "+rep   
    return infoString[5:]

def hgParent(rep):
    fName=tempfile.mktemp()
    os.system('hg show > '+fName)
    infoFile=open(fName)
    infoString=''
    headAttribute='paths.default='
    while 1:
        infoString=infoFile.readline()
        if (infoString[:len(headAttribute)]==headAttribute or len(infoString)==0):
            break
    infoFile.close()
    if (infoString[:len(headAttribute)]!=headAttribute):
        raise RuntimeError, "cannot find url for "+rep   
    return infoString[len(headAttribute):]

def noPending(rep):
    sys.stdout.write('\t')

def cvsChanged(rep):
    fName=tempfile.mktemp()
    os.environ['CVS_RSH']=os.environ['HOME']+'/bin/ssh_rice_utke'
    os.system('cvs -n update 2>&1 | grep -v \'targ_ia32\' | grep -v \'cvs update: Updating\' > '+fName)
    info=os.stat(fName)
    if info[6]>0 :
        sys.stdout.write('\tC')
    else:
        sys.stdout.write('\t')

def svnChanged(rep):
    fName=tempfile.mktemp()
    os.system('svn status > '+fName)
    info=os.stat(fName)
    if info[6]>0 :
        sys.stdout.write('\tC')
    else:
        sys.stdout.write('\t')

def hgChanged(rep):
    fName=tempfile.mktemp()
    os.system('hg status > '+fName)
    info=os.stat(fName)
    if info[6]>0 :
        sys.stdout.write('\tC')
    else:
        sys.stdout.write('\t')

def hgPending(rep):
    fName=tempfile.mktemp()
    os.system('hg outgoing > '+fName)
    infoFile=open(fName)
    outgoing=infoFile.readlines()
    infoFile.close()    
    os.system('hg incoming > '+fName)
    infoFile=open(fName)
    incoming=infoFile.readlines()
    infoFile.close()
    status=""
    if (re.search('no changes found',outgoing[2]) is None):
        status="O"
    if (re.search('no changes found',incoming[2]) is None):
        if (status == "0") :  
            status="P"
        else:    
            status="I"
    sys.stdout.write('\t'+status)

def main():
  usage = '%prog [options]'
  opt = OptionParser(usage=usage)
  opt.add_option('-q','--quick',dest='quick',
                 help="don't check for uncommited changes or pending pushes",
                 action='store_true',default=False)
  (options, args) = opt.parse_args()
  doQuick=False
  if (options.quick):
    doQuick=True
  cwd=os.getcwd()
  if (os.path.basename(cwd) != 'OpenAD'):
      print 'the current working directory has to the the OpenAD root directory'
      return -1
  repList=['../OpenAD','angel','boost','Open64','OpenADFortTk','OpenAnalysis','xaif','xaifBooster','xercesc']
  maxNameLength=max([len(r) for r in repList])
  repDict={}
  repDict['cvs']=(cvsParent,cvsChanged,noPending)
  repDict['svn']=(svnParent,svnChanged,noPending)
  repDict['hg']=(hgParent,hgChanged,hgPending)
  try:
      for i in repList:
          repK=repKind(i)
          os.chdir(i)
          info=repDict[repK][0](i)
          patchedName=i+"".join([' ' for s in range(len(i),maxNameLength)])
          sys.stdout.write('%s\t%s\t' % (patchedName,repK))
          if (re.search('anon',info) is None):
              sys.stdout.write('W')
              if (not doQuick) :
                  repDict[repK][1](i)
                  repDict[repK][2](i)
          else:
              sys.stdout.write('R')
              if (not doQuick) :
                  sys.stdout.write('\t\t')
          sys.stdout.write('\t%s\n' % info.strip())
          os.chdir(cwd)
  except RuntimeError, e:
      print 'caught excetion: ',e
      return -1

if __name__ == "__main__":
  sys.exit(main())
