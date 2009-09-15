#!/usr/bin/env python
##########################################################
# This file is part of OpenAD released under the LGPL.   #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the OpenAD distribution             #
##########################################################

import os
import sys
import traceback

#############################################################################
##
##   This module contains routines to dynamically generate shell
##   commands that modify one's environment.  Much effort has been
##   invested to ensure that the output can either be sent to a file
##   and sourced, or, more preferably, used with eval:
##     eval `generate_commands ...`
##   Much effort has been needed to repress many dark thoughts
##   about csh.
##
#############################################################################


#############################################################################
## Subroutines
#############################################################################

# genSetEnvVar: Given a variable name 'var', its value 'value' and a
#   shell type (sh or csh), generate a string that will set that value
#   and export it to the environment with syntax for the given shell.
#   If 'value' is equal to undef, then the variable will be *unset*.
# returns: a string
# effect: nothing
# assumes: nothing

class libsetenv:
  def __init__(self, shell):
    self.shell = shell

  def genSetEnvVar(self, var, value):
    # Return an unset of $value is undefined
    try:
      str = ""
      if (self.is_sh()):
        str += var+"=\""+value+"\";\n"
        str += "export "+var+";\n"
      elif(self.is_csh()):
        str += "setenv "+var+" \""+value+"\";"
      else:
        sys.stderr.write("Programming Error!")
        sys.exit()
    
    except NameError:
      return self.genUnSetEnvVar(var)
    return str
 
# genAppendEnvVar: Given a variable name 'var', its value 'value' and
#   a shell type (sh or csh), generate a string that will append that
#   value to the exported variable.  If 'value' is equal to undef, the
#   empty string will be returned.
# returns: a string
# effect: nothing
# assumes: nothing
  def genAppendEnvVar(self,var,value):
    try:
      str=""
      if(self.is_sh()):
        test="-z \""+var+"\"" #"-z" checks if var is defined
      elif (self.is_csh()):
        test="! $?"+var # "$?" returns 1 if var name is set
      else:
        sys.stderr.write("Programming Error!")
        sys.exit()
      
      appval = value + ':${'+var+'}'
      thn = self.genSetEnvVar(var, value)
      els = self.genSetEnvVar(var, appval)
      str += self.genIf(test, thn, els)
    except NameError:
      return ""
    return str

# genUnSetEnvVar: Given a variable name 'var' and a shell type (sh or
#   csh), generate a string that will unset that value with syntax for
#   the given shell.
# returns: a string
# effect: nothing
# assumes: nothing
  def genUnSetEnvVar(self,var):
    str=""
    if(self.is_sh()):
      str +="unset "+var
    elif(self.is_csh()):
      str += "unsetenv "+var
    else:
      sys.stderr.write("Programming Error!")
      sys.exit()
    return str

# genPrintEnvVar: Given a variable name 'var' and a shell type (sh or
#   csh), generate a string that will print that value with syntax for
#   the given shell.
# returns: a string
# effect: nothing
# assumes: nothing
  def genPrintEnvVar(self,var):
    simplePrint = "echo \""+var+" = \$"+var+"\""
    str = ""
    if(self.is_sh()):
      str += simplePrint + "\n"
    elif(self.is_csh()):
      test = "$?"+var
      thn = " echo \""+var+" = \"\n"
      els = " "+simplePrint+"\n"
      str += self.genIf(test, thn, els)
    else:
      sys.stderr.write("Programming Error!")
      sys.exit()
    return str

# genSetAlias: Generate an alias appropriate to the shell.  If 'value' is
# equal to undef, then the alias will be *unset*.
# returns: a string
# effect: nothing
# assumes: nothing
  def genSetAlias(self, alias, value):
     # Return an unset of $value is undefined
     # Otherwise return an alias
    try:
      str=""
      if(self.is_sh()):
        str += "alias "+alias+"=\""+value+"\";"
      elif(self.is_csh()):
        str += "alias "+alias+" \""+value+"\";"
      else:
        sys.stderr.write("Programming Error!")
        sys.exit()
    except NameError:
      return self.genUnSetAlias(alias)
    return str

# genUnSetAlias: Generate an alias unset appropriat to the shell.
# returns: a string
# effect: nothing
# assumes: nothing
  def genUnSetAlias(self, alias):
    str=""
    if(self.is_sh() or self.is_csh()):
      str += "unalias "+alias
    else:
      sys.stderr.write("Programming Error!")
      sys.exit()
    return str



# genIf: Generate an if conditional.  If 'else' is undefined, do not
#   generate the else part.  The 'test', 'then' and 'else' should be
#   appropriate for shell. 
# csh has especially braindead if-syntax because 'else' and 'endif'
#   must be on separate lines -- an impossibility with the eval
#   `command...` idiom.  The only form of csh 'if' that reliably works
#   both in a script and within eval is
#     if (expr) cmd
#   Adding an eval provides support for multiple commands:
#     if (expr) eval <commands>
#     if (!expr) eval <commands>
# Note: One can use the following quoting scheme (based on "Unix Power
#   Tools, 10.7") to embed an if in an eval.  Howver, when the output is
#   filtered through backquotes, the newlines are eliminated creating a
#   parse error (else is no longer the first token on a line).
#   
#     if ( ! $?XXX ) then   --->   eval "if ( ! $?XXX ) then \
#       echo "XXX = "                echo "XXX = " \
#     else                         else \
#       echo "XXX = ${XXX}"          echo "XXX = ${XXX}" \
#     endif                        endif"
#
# returns: a string
# effect: nothing
# assumes: nothing

  def genIf(self, test, _then, _else):
    str = ""
    if (self.is_sh()):
      str += 'if [ '+test+' ]; then'+'\n'
      str += _then.rstrip()+"\n"
      try:
        str +='else\n' + _else.rstrip()
      except NameError:
        pass
      str += '\nfi;' + "\n"
    elif(self.is_csh()):
      str += 'if ( '+test+' ) '+ "eval '"+_then.rstrip()+" ';\n"
      str += 'if ( ! '+test+' ) '+"eval '"+_else.rstrip()+" ';\n"
    else:
      sys.stderr.write("Programming Error!")
      sys.exit()
    return str


# is_sh: Given the shell type, return true if we can use sh syntax
  def is_sh(self):
    return(self.shell == 'sh' or self.shell == 'bash' or self.shell == 'ksh')

# is_csh: Given the shell type, return true if we can use csh syntax
  def is_csh(self):
    return(self.shell == 'csh' or self.shell == 'tcsh')
