#! /usr/bin/env perl
# -*-Mode: perl;-*-

# -*-Mode: perl;-*-
# make sure we execute perl
#eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}'
#& eval 'exec perl -S $0 $argv:q'
#if 0;

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/tools/setenv/setenv.pl,v 1.1 2004-05-20 21:39:28 eraxxon Exp $
## * BeginCopyright *********************************************************
## 
## 
## *********************************************************** EndCopyright *

#############################################################################
##
## $Source: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/tools/setenv/setenv.pl,v $ 
##
##   Nathan Tallent.
##
#############################################################################

use strict;
use warnings;

use FindBin qw($RealBin);
use IO::File;
use Getopt::Long;

use lib "$RealBin/../libperl";
use setenv qw(getSetEnvVar genUnSetEnvVar genPrintEnvVar is_sh is_csh);

use lib "$RealBin/../..";
use openad_config;


STDOUT->autoflush(1); 

#############################################################################

my $the_program = "setenv.pl";
my $the_usage = 
"usage: ${the_program} [options] <shell>

  Required arguments:
    -s, --shell=<shell-type> : sh | csh

  General options:
    -h, --help : print help
\n";

my @the_options = ('shell|s=s',
		   'help|h',
		   );

#############################################################################

#############################################################################
## main/driver
#############################################################################

my $the_homedir = $ENV{'HOME'};

my $opt_shell = undef;
my $opt_category = undef;


# ----------------------------------------------------------
# Parse the command line
# ----------------------------------------------------------

parseOptions($0);

# ----------------------------------------------------------
# Run
# ----------------------------------------------------------

GenEnvSettings($opt_shell);

#############################################################################
## GenEnvSettings
#############################################################################

# GenEnvSettings: Generate code to setup CVS repository variables
# returns: 
# effect: 
# assumes:
sub GenEnvSettings
{
  my($shell) = @_;
  
  # --------------------------------------------------------
  # Generate commands for sub repositories
  # --------------------------------------------------------
  my $config = openad_config->new();
  my $OpenADRepos = $config->getRepos();
  
  for my $repo (@{$OpenADRepos}) {
    my $repoPath = $repo->{path} . '/' . $repo->{name};
    my $var = $repo->{var};
    my $val = $repoPath;
    print STDOUT getSetEnvVar($var, $val, $shell);
    #print STDOUT genPrintEnvVar($var, $shell);
  }
  
}

#############################################################################
## parseOptions
#############################################################################

sub parseOptions
{
  my ($command) = @_;
  
  # Get optional arguments
  my %opts = ();
  my $ret = GetOptions(\%opts, @the_options);
  if (!$ret) { 
    printErrorAndExit(); 
  }
  
  # Get optional arguments: help
  if (defined( $opts{'help'} )) {
    printUsageAndExit($command);
  }
  
  # ----------------------------------------------------------
  # Required arguments
  # ----------------------------------------------------------

  # Shell type
  if (defined($opts{'shell'})) {
    $opt_shell = $opts{'shell'};
  } else {
    printErrorAndExit("Shell option missing.\n");
  }
  if (! (is_sh($opt_shell) || is_csh($opt_shell)) ) {
    printErrorAndExit("Invalid shell '$opt_shell'.\n");
  }
  
  ## Make sure called with minimum number of required arguments
  my $numArgs = scalar(@ARGV);
  if ($numArgs != 0) { 
    printErrorAndExit("Invalid number of required arguments!\n");
  }
  
  #print STDOUT "shell:           $opt_shell\n";
}

# printUsageAndExit
sub printUsageAndExit 
{
  my ($command) = @_; # not used now
  print STDOUT ${the_usage};
  exit(-1);
}

# printErrorAndExit
sub printErrorAndExit 
{
  my ($msg) = @_;
  if (defined($msg)) {
    print STDOUT "${msg}";
  }
  print STDOUT "Try `${the_program} --help' for more information.\n";
  exit(-1);
}

#############################################################################

# Local Variables:
# perl-indent-level: 2
# End:
