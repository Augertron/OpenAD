# -*-Mode: perl;-*-

## * BeginCopyright *********************************************************
## 
## 
## *********************************************************** EndCopyright *

#############################################################################
##
## $Source: /Volumes/cvsrep/developer/shared/libperltk/RepositoryTools.pm,v $ 
##
##   Nathan Tallent.
##
#############################################################################

package RepositoryTools;

use strict;
use warnings;

use Cwd;
use IO::File;

use RunCmds qw(%CmdDesc RunCmds RunCmd);

use base qw(Exporter);
use vars qw($VERSION @EXPORT @EXPORT_OK);
$VERSION = '0.001';
@EXPORT_OK = qw(%RepositoryDesc %CVSReposDesc %BKReposDesc 
		RunRepositoryUpdate);

STDOUT->autoflush(1); 

#############################################################################

%RepositoryTools::RepositoryDesc = 
    (name      => undef, # name
     path      => undef, # path to repository
     subdir    => undef, # subdirectory of repository (optional)
     repos     => undef, # if undef, considered 'external'; user must manage
     tag       => undef, # interpreted as a tag/branch by default;
                         # if '{date}' is prepended, then a cvs date
     var       => undef, # corresponding environment variable
     );

%RepositoryTools::CVSReposDesc = 
    (iscvs        => 1,
     rsh          => undef,
     root         => undef,
     );

%RepositoryTools::BKReposDesc = 
    (isbk          => 1,
     root          => undef,
     );


#############################################################################

# RunRepositoryUpdate: Given several arguments, perform a get or
# update on a repository that itself contains other cvs or bitkeeper
# repositories.
#   selfRoot     - the root of the shell CVS repository
#   selfCVSFiles - list of shell globs representing the shell CVS repository
#   repositories - list of 'RepositoryDesc'
#   opts         - a hash of options
#     'skipupdate'  - do not update an existing repository (boolean)
#     'interactive' - run commands interactively (boolean)
#     'verbose'     - 0 (no), 1 (moderate) or 2 (extreme)
#     'logfnm'      - if defined, the name of a log file to which all output
#                     should be sent
#     'debug'       - if defined, a debug level from 0 - 9
sub RunRepositoryUpdate 
{
  my($selfRoot, $selfCVSFiles, $repositories, $opts) = @_;
  
  my $cmdDescVecRef = [ ];
  my $desc = undef;
  
  my $skipupdate = (defined($opts->{skipupdate})) ? $opts->{skipupdate} : 0;
  my $interactv  = (defined($opts->{interactive})) ? $opts->{interactive} : 0;
  my $verbose    = (defined($opts->{verbose})) ? $opts->{verbose} : 0;
  my $debug      = (defined($opts->{debug})) ? $opts->{debug} : 0;

  # --------------------------------------------------------
  # Generate commands for self (always a cvs update)
  # --------------------------------------------------------

    my $selfFiles = '';
    for my $x (@{$selfCVSFiles}) {
      $selfFiles .= ' ' . $x;
    }
    
    $desc = { %RunCmds::CmdDesc, };  
    my $env = 'CVS_RSH="' . "${selfRoot}/tools/sshcvs/sshcvs-hipersoft-anon" .  '"';
    $desc->{cmd} = "cd ${selfRoot} && ${env} cvs update ${selfFiles}";
    $desc->{desc} = $desc->{cmd};
    push(@{$cmdDescVecRef}, $desc); 

  # --------------------------------------------------------
  # Generate commands for sub repositories
  # --------------------------------------------------------
  
    for my $repo (@{$repositories}) {
      # if we don't have repository info, then this is external to us
      next if (!defined($repo->{repos}));
      
      my $localRepoPath = $repo->{path} . '/' . $repo->{name};
      if (defined($repo->{subdir})) { 
	$localRepoPath = $localRepoPath . '/' . $repo->{subdir}
      }
      my $repoExists = (-d $localRepoPath);
      my $repoTag = (defined($repo->{tag})) ? $repo->{tag} : "default";

      next if ($repoExists && $skipupdate);
      
      # Either checkout or update the repository
      $desc = { %RunCmds::CmdDesc, };
      
      if (defined($repo->{repos}->{iscvs})) {
	# A CVS repository
	my $nm = $repo->{name};
	my $env = 'CVS_RSH="' . $repo->{repos}->{rsh} . '"';
	my $opt = '-z5 -d ' . $repo->{repos}->{root};
	my $tag = $repoTag;
	
	if ($repoExists) {
	  $desc->{cmd} = "cd ${localRepoPath} && ${env} cvs ${opt} update -d";
	}
	else {
	  if (defined($repo->{subdir})) { 
	    $nm = $nm . '/' . $repo->{subdir}
	  }
	  my $topt = getCVSTagOpt($repo->{tag});
	  $desc->{cmd} = "cd $repo->{path} && " .
	      "${env} cvs ${opt} co ${topt} ${nm}";
	}
	$desc->{desc} = $desc->{cmd};
      } 
      elsif (defined($repo->{repos}->{isbk})) {
	# A BitKeeper repository
	my $nm = $repo->{name};
	my $arg = $repo->{repos}->{root};
	my $tag = $repoTag;
	
	if ($repoExists) {
	  $desc->{cmd} = "cd $repo->{path} && update ${arg} $nm";
	  $desc->{desc} = "update ${arg} $nm";
	}
	else {
	  $desc->{cmd} = "cd $repo->{path} && sfioball ${arg} $nm";
	  $desc->{desc} = "sfioball ${arg} $nm";
	}
      }
      else {
	die "Programming Error!";
      }
      
      push(@{$cmdDescVecRef}, $desc);
    }
  
  # --------------------------------------------------------
  # Run commands
  # --------------------------------------------------------
  if ($debug) {
    print STDOUT "--> ", $opts->{logfnm}, "\n";
  }

  RunCmds($cmdDescVecRef, $verbose, $interactv, $opts->{logfnm});
}


# getCVSTagOpt: 
sub getCVSTagOpt
{
  my($tag) = @_;
  
  my $opt = "";
  
  if (defined($tag)) {
    my $date = "";
    my $re = '^{date}(.*)';
    if (($date) = $tag =~ /$re/) {
      $opt = "-D " . $date;
    }
    else {
      $opt = "-r " . $tag;
    }
  }

  return $opt;
}

#############################################################################

# Local Variables:
# perl-indent-level: 2
# End:
