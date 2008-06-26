# -*-Mode: perl;-*-

# $Header: /Volumes/cvsrep/developer/shared/libperltk/setenv.pm,v 1.4 2004/06/08 14:36:40 eraxxon Exp $
## * BeginCopyright *********************************************************
## 
## 
## *********************************************************** EndCopyright *

#############################################################################
##
## $Source: /Volumes/cvsrep/developer/shared/libperltk/setenv.pm,v $ 
##
##   Nathan Tallent.
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

package setenv;

use strict;
use warnings;

use base qw(Exporter);
use vars qw($VERSION @EXPORT @EXPORT_OK);
$VERSION = '0.001';
@EXPORT_OK = qw(genSetEnvVar genSetVar genAppendEnvVar 
		genUnSetEnvVar genUnSetVar genPrintEnvVar genPrintVar
		genSetAlias genUnSetAlias genIf
		is_sh is_csh);

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
sub genSetEnvVar
{
  my($var, $value, $shell) = @_;
  
  # Return an unset of $value is undefined
  if (!defined($value)) { 
    return genUnSetEnvVar($var, $shell);
  }
  
  # Otherwise return a variable assignment
  my $str = "";
  if (is_sh($shell)) {
    $str .= "${var}=\"${value}\";\n";
    $str .= "export ${var};\n";
  } elsif (is_csh($shell)) {
    $str .= "setenv ${var} \"${value}\";\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genSetVar: Same as above but the variable is not exported to the
#   environment.
# returns: a string
# effect: nothing
# assumes: nothing
sub genSetVar
{
  my($var, $value, $shell) = @_;
  
  # Return an unset of $value is undefined
  if (!defined($value)) { 
    return genUnSetVar($var, $shell);
  }
  
  # Otherwise return a variable assignment
  my $str = "";
  if (is_sh($shell)) {
    $str .= "${var}=\"${value}\";\n";
  } elsif (is_csh($shell)) {
    $str .= "set ${var}=\"${value}\";\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genAppendEnvVar: Given a variable name 'var', its value 'value' and
#   a shell type (sh or csh), generate a string that will append that
#   value to the exported variable.  If 'value' is equal to undef, the
#   empty string will be returned.
# returns: a string
# effect: nothing
# assumes: nothing
sub genAppendEnvVar
{
  my($var, $value, $shell) = @_;
  
  if (!defined($value)) { 
    return "";
  }
  
  my $str = "";
  my $test = undef;
  if (is_sh($shell)) {
    $test = "-z \"${var}\"";
  } elsif (is_csh($shell)) {
    $test = '! $?' . $var;
  } else {
    die "Programming Error!";
  }
  
  my $appval = $value . ':${' . $var . '}';
  my $thn = genSetEnvVar($var, $value, $shell);
  my $els = genSetEnvVar($var, $appval, $shell);
  $str .= genIf($test, $thn, $els, $shell);
  
  return $str;
}


# genUnSetEnvVar: Given a variable name 'var' and a shell type (sh or
#   csh), generate a string that will unset that value with syntax for
#   the given shell.
# returns: a string
# effect: nothing
# assumes: nothing
sub genUnSetEnvVar
{
  my($var, $shell) = @_;
  
  my $str = "";
  if (is_sh($shell)) {
    $str .= "unset ${var};\n";
  } elsif (is_csh($shell)) {
    $str .= "unsetenv ${var};\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genUnSetVar: Same as above but for a variable that has not been
#   exported to the environment.
# returns: a string
# effect: nothing
# assumes: nothing
sub genUnSetVar
{
  my($var, $shell) = @_;
  
  my $str = "";
  if (is_sh($shell) || is_csh($shell)) {
    $str .= "unset ${var};\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genPrintEnvVar: Given a variable name 'var' and a shell type (sh or
#   csh), generate a string that will print that value with syntax for
#   the given shell.
# returns: a string
# effect: nothing
# assumes: nothing
sub genPrintEnvVar
{
  my($var, $shell) = @_;
  
  my $simplePrint = "echo \"$var = \${$var}\"";
  my $str = "";
  
  if (is_sh($shell)) {
    $str .= $simplePrint . ";\n";
  } elsif (is_csh($shell)) {
    my $test = '! $?' . $var;
    my $thn = "  echo \"$var = \";\n";
    my $els = "  $simplePrint ;\n";
    $str .= genIf($test, $thn, $els, $shell);
  } else {
    die "Programming Error!";
  }
  return $str;
}


# genPrintVar: Same as above but for a variable that has not been
#   exported to the environment.
# returns: a string
# effect: nothing
# assumes: nothing
sub genPrintVar
{
  my($var, $shell) = @_;
  return genPrintEnvVar($var, $shell);
}


# genSetAlias: Generate an alias appropriate to the shell.  If 'value' is
# equal to undef, then the alias will be *unset*.
# returns: a string
# effect: nothing
# assumes: nothing
sub genSetAlias
{
  my($alias, $value, $shell) = @_;
  
  # Return an unset of $value is undefined
  if (!defined($value)) { 
    return genUnSetAlias($alias, $shell);
  }
  
  # Otherwise return an alias
  my $str = "";
  if (is_sh($shell)) {
    $str .= "alias ${alias}=\"${value}\";\n";
  } elsif (is_csh($shell)) {
    $str .= "alias ${alias} \"${value}\";\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genUnSetAlias: Generate an alias unset appropriat to the shell.
# returns: a string
# effect: nothing
# assumes: nothing
sub genUnSetAlias
{
  my($alias, $shell) = @_;
  
  my $str = "";
  if (is_sh($shell) || is_csh($shell)) {
    $str .= "unalias ${alias};\n";
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# genIf: Generate an if conditional.  If 'else' is undefined, do not
#   generate the else part.  The 'test', 'then' and 'else' should be
#   appropriate for shell. 
# csh has especially braindead if-syntax because 'else' and 'endif'
#   must be on separate lines -- an impossibility with the eval
#   `command...` idiom.  The only form of csh 'if' that reliably works
#   both in a script in within eval is
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
sub genIf
{
  my($test, $_then, $_else, $shell) = @_;

  my $str = "";
  if (is_sh($shell)) {
    $str .= 'if [ ' . $test  . ' ]; then' . "\n";
    $str .= $_then;
    if (defined($_else)) {
      $str .= 'else' . "\n" . $_else;
    }
    $str .= 'fi' . ";\n";
  } elsif (is_csh($shell)) {
    $str .= 'if ( ' . $test  . ' ) ' . "eval '" . $_then . "';\n";
    if (defined($_else)) {
      $str .= 'if ( ! ' . $test  . ' ) ' . "eval '" . $_else . "';\n";
    }
  } else {
    die "Programming Error!";
  }
  
  return $str;
}


# is_sh: Given the shell type, return true if we can use sh syntax
sub is_sh
{
  my($shell) = @_;
  return ($shell eq 'sh' || $shell eq 'bash' || $shell eq 'ksh');
}


# is_csh: Given the shell type, return true if we can use csh syntax
sub is_csh
{
  my($shell) = @_;
  return ($shell eq 'csh' || $shell eq 'tcsh');
}


#############################################################################

# Local Variables:
# perl-indent-level: 2
# End:
