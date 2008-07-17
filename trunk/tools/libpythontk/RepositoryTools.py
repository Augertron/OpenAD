#!usr/bin/env python
import os
import sys
import RunCmds
import Repository
from RunCmds import CmdDesc

#############################################################################

# RunRepositoryUpdate: Given several arguments, perform a get or
# update on a repository that itself contains other cvs or bitkeeper
# repositories.
#   selfRoot     - the root of the shell CVS repository
#   repositories - list of 'RepositoryDesc'
#   opts         - a hash of options
#     'skipupdate'  - do not update an existing repository (boolean)
#     'interactive' - run commands interactively (boolean)
#     'verbose'     - 0 (no), 1 (moderate) or 2 (extreme)
#     'logfnm'      - if defined, the name of a log file to which all output
#                     should be sent
#     'debug'       - if defined, a debug level from 0 - 9


class RepositoryTools:

  def __init__(self):
    None

  def RunRepositoryUpdate(self, selfRoot, repositories, opts):
    cmdDescVecRef = []
    desc = 'undef'
    optsKeys = {'skipupdate': 0,
                'interactive':0,
                'verbose':0,
                'debug':0}
  
    for k in optsKeys.keys():
      if opts[k] != 0:
        optsKeys[k] = opts[k]
      else:
        val = 0

  # --------------------------------------------------------
  # Generate commands for sub repositories
  # --------------------------------------------------------

    for repo in repositories.values():
       # if we don't have repository info, then this is external to us
      if repo.getRoot() == None:
        continue
      if (repo.repoExists() and optsKeys['skipupdate']):
        continue
      if repo.repoExists():
        repo.update() # sets CmdDesc to update repository
      else:
        if repo.getSubdir() is not None:
          repo.checkoutSubdir() # sets CmdDesc to update subdir repository
        else:
          repo.checkout() # check out repository
      cmdDescVecRef.append(repo.cmdDesc)

  # --------------------------------------------------------
  # Run commands
  # --------------------------------------------------------
    if opts['debug'] != 0:
      sys.stdout.write("--> "+opts['logfnm']+"\n")
    RunCmds.RunCmds().RunCmds(cmdDescVecRef, opts['verbose'], opts['interactive'], opts['logfnm'])

############################################################################
