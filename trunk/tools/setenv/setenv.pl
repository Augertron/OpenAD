#! /usr/bin/env perl
# -*-Mode: perl;-*-

# -*-Mode: perl;-*-
# make sure we execute perl
#eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}'
#& eval 'exec perl -S $0 $argv:q'
#if 0;

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/tools/setenv/setenv.pl,v 1.2 2004-05-21 13:54:10 eraxxon Exp $
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
use Cwd qw(abs_path);
use IO::File;
use Getopt::Long;

use lib "$RealBin/../libperl";
use setenv qw(genSetEnvVar genSetVar genAppendEnvVar 
	      genUnSetEnvVar genUnSetVar genPrintEnvVar genPrintVar
	      genSetAlias genUnSetAlias genIf
	      is_sh is_csh);

use lib "$RealBin/../..";
use openad_config;

my $OpenADRoot = abs_path("$RealBin/../..");

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

my %platformToOpen64TargTable;
my %varidx;
my @RootEnvVars;
my @OtherEnvVars;
my @Aliases;

BEGIN {

  %platformToOpen64TargTable =
      (
       'alpha-OSF1'  => 'targ_alpha_tru64', 
       'i686-Linux'  => 'targ_ia32_ia64_linux',
       'i686-Cygwin' => 'targ_ia32_ia64_linux',
       'ia64-Linux'  => 'targ_ia64_ia64_linux',
       'mips-IRIX64' => 'targ_mips_irix',
       'sparc-SunOS' => 'targ_sparc_solaris',
       );
  
  %varidx = ( 'var' => 0, 'val' => 1, );
  
  @RootEnvVars =
      (
       ['OPEN64ROOT',       '${OPEN64_BASE}/osprey1.0/${o64targ}'],
       ['OPENANALYSISROOT', '${OPENANALYSIS_BASE}/${PLATFORM}'],
       ['XERCESCROOT',      '${XERCESC_BASE}/${PLATFORM}'],
       ['XAIFBOOSTERROOT',  '${XAIFBOOSTER_BASE}'],
       ['BOOSTROOT',        '${BOOST_BASE}'],
       ['ANGELROOT',        '${ANGEL_BASE}'],
       ['XAIFSCHEMAROOT',   '${XAIFSCHEMA_BASE}'],
       [ undef,             undef ],
       );
  
  @OtherEnvVars =
      (
       ['OPENADFORTTK_OPEN64BASE', '${OPEN64_BASE}/osprey1.0'],
       ['OPENADFORTTK_OPEN64',     '${OPEN64ROOT}'],
       ['OPENADFORTTK_OA',         '${OPENANALYSISROOT}'],
       ['OPENADFORTTK_XERCESC',    '${XERCESCROOT}'],
       ['OPENADFORTTK_BOOST',      '${BOOSTROOT}'],
       [ undef,                    undef],
       [ 'XAIF_DIR',               '${XAIFBOOSTERROOT}' ],
       [ 'BOOST_DIR',              '${BOOSTROOT}' ],
       [ undef,                    undef],
       );
  
  @Aliases =
      (
       ['mfef90',    '${OPEN64ROOT}/crayf90/sgi/mfef90' ],
       ['whirl2f',   '${OPEN64ROOT}/whirl2f/whirl2f' ],
       ['whirl2f90', '${OPEN64ROOT}/whirl2f/whirl2f90'],
       ['ir_b2a',    '${OPEN64ROOT}/ir_tools/ir_b2a' ],
       ['ir_size',   '${OPEN64ROOT}/ir_tools/ir_size'],
       );
}

# GenEnvSettings: Generate code to setup CVS repository variables
# returns: 
# effect: 
# assumes:
sub GenEnvSettings
{
  my($shell) = @_;
  
  # --------------------------------------------------------
  # Generate canonical platform
  # --------------------------------------------------------
  my $platform = 'i686-Linux'; # `cd ${OpenADRoot}/config; ./hpcplatform`;
  print STDOUT genSetVar('PLATFORM', $platform, $shell);

  my $o64targ = $platformToOpen64TargTable{$platform};
  if (!defined($o64targ)) {
    die "Programming error: Unknown platform!\n";
  }
  print STDOUT genSetVar('o64targ', $o64targ, $shell);

  # --------------------------------------------------------
  # Generate BASE vars for sub repositories
  # --------------------------------------------------------
  print STDOUT "\n";

  my $config = openad_config->new();
  my $OpenADRepos = $config->getRepos();

  for my $repo (@{$OpenADRepos}) {
    my $repoPath = $repo->{path} . '/' . $repo->{name};
    if (-d $repoPath) {
      my $var = $repo->{var};
      my $val = $repoPath;
      print STDOUT genSetEnvVar($var, $val, $shell);
      #print STDOUT genPrintEnvVar($var, $shell);
    }
  }

  # --------------------------------------------------------
  # Generate environment vars for sub repositories
  # --------------------------------------------------------
  print STDOUT "\n";

  my @EnvVars = (@RootEnvVars, @OtherEnvVars);
  for my $pair (@EnvVars) {
    my $var = $pair->[$varidx{var}];
    my $val = $pair->[$varidx{val}];
    if (defined($var)) {
      print STDOUT genSetEnvVar($var, $val, $shell);
    } else {
      print STDOUT "\n";
    }
  }

  # --------------------------------------------------------
  # Generate other environment stuff
  # --------------------------------------------------------
  
  if ($platform eq 'i686-Cygwin') {
    my $path = '${XERCESCROOT}/bin:${XERCESCROOT}/lib:${OPEN64ROOT}/be:${OPEN64ROOT}/whirl2f:${PATH}';
    print STDOUT genAppendEnvVar('PATH', $path, $shell);
  } else {
    my $ldlib = '${XERCESCROOT}/lib:${OPEN64ROOT}/whirl2f';
    print STDOUT genAppendEnvVar('LD_LIBRARY_PATH', $ldlib, $shell);
  }    
  
  
  # --------------------------------------------------------
  # Generate aliases
  # --------------------------------------------------------
  print STDOUT "\n";

  for my $pair (@Aliases) {
    my $var = $pair->[$varidx{var}];
    my $val = $pair->[$varidx{val}];
    print STDOUT genSetAlias($var, $val, $shell);
  }
  
  # --------------------------------------------------------

  print STDOUT "\n";
  print STDOUT genUnSetVar('PLATFORM', $shell);
  print STDOUT genUnSetVar('o64targ', $shell);


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
