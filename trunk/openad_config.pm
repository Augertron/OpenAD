# -*-Mode: perl;-*-
# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/openad_config.pm,v 1.10 2004-10-18 15:23:54 utke Exp $

#############################################################################
##
## OpenAD configuration information for subpackages.
##
## This is a Perl module masquerading as a configuration file.
## Figuring this out should be pretty easy, but if you are making
## changes to the defaults, we presume you know what you are doing or
## can handle the consequences.
##
#############################################################################

package openad_config;

use strict;
use warnings;

use FindBin qw($RealBin);
use Cwd qw(abs_path);

# FindBin will return the location of the *calling* script and we can't
# yet assume FindBin::again() is widely available. It should be safe to
# assume caller is located in ./tools/xxx/
my $TheRealBin = abs_path("$RealBin/../..");

#use lib "$TheRealBin/tools/libperltk";
use lib "$RealBin/../../tools/libperltk";
use RepositoryTools qw(%RepositoryDesc %CVSReposDesc %BKReposDesc);

use base qw(Exporter);
use vars qw(@EXPORT @EXPORT_OK);
#@EXPORT_OK = qw();

#############################################################################
## Subpackage configuration information
#############################################################################

######################################################################
# Repository information
######################################################################

# USER is not available in some Cygwin environments
my $defaultUser = (defined($ENV{'USER'})) ? $ENV{'USER'} : $ENV{'USERNAME'};

##################################################
# Rice HiPerSoft CVS Repository
##################################################

my $riceUser = 'anoncvs'; # $defaultUser

my $OPENAD_REPO_RICECVS = { %RepositoryTools::CVSReposDesc, }; 
$OPENAD_REPO_RICECVS->{rsh} = 
    "$TheRealBin/tools/sshcvs/sshcvs-hipersoft-anon";
$OPENAD_REPO_RICECVS->{root} = 
    ':ext:' . $riceUser . '@koolkat2.cs.rice.edu:/Volumes/cvsrep/developer';

##################################################
# Argonne Bitkeeper
##################################################

#my $anlUser = $defaultUser;

#my $OPENAD_REPO_ANLBK_XB = { %RepositoryTools::BKReposDesc, }; 
#$OPENAD_REPO_ANLBK_XB->{root} =
#   $anlUser . '@terra.mcs.anl.gov:/home/derivs/share/xaifBooster';

#my $OPENAD_REPO_ANLBK_ANGEL = { %RepositoryTools::BKReposDesc, }; 
#$OPENAD_REPO_ANLBK_ANGEL->{root} = $anlUser . 
#    '@terra.mcs.anl.gov:/home/utke/BK_Reps/CODE/angel';

##################################################
# Bkbits Bitkeeper Repositories
##################################################

my $OPENAD_REPO_BK_XB = { %RepositoryTools::BKReposDesc, }; 
$OPENAD_REPO_BK_XB->{root} = 'http://xaifbooster.bkbits.net/xaifBooster';

my $OPENAD_REPO_BK_XAIF = { %RepositoryTools::BKReposDesc, }; 
$OPENAD_REPO_BK_XAIF->{root} = 'http://xaif.bkbits.net/xaif-1.0';

##################################################
# SourceForge CVS Repositories
##################################################

my $OPENAD_REPO_SF_ANGEL = { %RepositoryTools::CVSReposDesc, }; 
$OPENAD_REPO_SF_ANGEL->{rsh} = 'pserver';
$OPENAD_REPO_SF_ANGEL->{root} = 
    ':pserver:anonymous@cvs.sourceforge.net:/cvsroot/angellib';

my $OPENAD_REPO_SF_BOOST = { %RepositoryTools::CVSReposDesc, }; 
$OPENAD_REPO_SF_BOOST->{rsh} = 'pserver';
$OPENAD_REPO_SF_BOOST->{root} = 
    ':pserver:anonymous@cvs.sourceforge.net:/cvsroot/boost';

######################################################################
# OpenAD Repositories, Local Instances
######################################################################  

my $OPENAD_OPEN64 = { %RepositoryTools::RepositoryDesc, };
$OPENAD_OPEN64->{name}  = 'Open64';
$OPENAD_OPEN64->{path}  = "$TheRealBin";
$OPENAD_OPEN64->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_OPEN64->{tag}   = 'OpenAD';
$OPENAD_OPEN64->{var}   = 'OPEN64_BASE';

my $OPENAD_OPENADFORTTK = { %RepositoryTools::RepositoryDesc, };
$OPENAD_OPENADFORTTK->{name}  = 'OpenADFortTk';
$OPENAD_OPENADFORTTK->{path}  = "$TheRealBin";
$OPENAD_OPENADFORTTK->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_OPENADFORTTK->{var}   = 'OPENADFORTTK_BASE';

my $OPENAD_OPENANALYSIS = { %RepositoryTools::RepositoryDesc, };
$OPENAD_OPENANALYSIS->{name}  = 'OpenAnalysis';
$OPENAD_OPENANALYSIS->{path}  = "$TheRealBin";
$OPENAD_OPENANALYSIS->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_OPENANALYSIS->{var}   = 'OPENANALYSIS_BASE';

my $OPENAD_XERCESC = { %RepositoryTools::RepositoryDesc, };
$OPENAD_XERCESC->{name}  = 'xercesc';
$OPENAD_XERCESC->{path}  = "$TheRealBin";
$OPENAD_XERCESC->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_XERCESC->{var}   = 'XERCESC_BASE';

my $OPENAD_XAIFBOOSTER = { %RepositoryTools::RepositoryDesc, };
$OPENAD_XAIFBOOSTER->{name}  = 'xaifBooster';
$OPENAD_XAIFBOOSTER->{path}  = "$TheRealBin";
$OPENAD_XAIFBOOSTER->{repos} = $OPENAD_REPO_BK_XB;
$OPENAD_XAIFBOOSTER->{var}   = 'XAIFBOOSTER_BASE';

my $OPENAD_ANGEL = { %RepositoryTools::RepositoryDesc, };
$OPENAD_ANGEL->{name}  = 'angel';
$OPENAD_ANGEL->{path}  = "$TheRealBin";
$OPENAD_ANGEL->{repos} = $OPENAD_REPO_SF_ANGEL;
$OPENAD_ANGEL->{var}   = 'ANGEL_BASE';

my $OPENAD_BOOST = { %RepositoryTools::RepositoryDesc, };
$OPENAD_BOOST->{name}  = 'boost';
$OPENAD_BOOST->{path}  = "$TheRealBin";
$OPENAD_BOOST->{repos} = $OPENAD_REPO_SF_BOOST;
$OPENAD_BOOST->{tag}   = 'Version_1_30_2';
$OPENAD_BOOST->{var}   = 'BOOST_BASE';

my $OPENAD_XAIF = { %RepositoryTools::RepositoryDesc, };
$OPENAD_XAIF->{name}  = 'xaif-1.0';
$OPENAD_XAIF->{path}  = "$TheRealBin";
$OPENAD_XAIF->{repos} = $OPENAD_REPO_BK_XAIF;
$OPENAD_XAIF->{var}  = 'XAIFSCHEMA_BASE';

#############################################################################
## Methods
#############################################################################

# A list of all repositories (RepositoryDesc) in this configuation of OpenAD

sub new {
  my $class = shift;
  my $self = { };
  bless $self, $class;
  $self->initialize();
  return $self;
}  

# getRepos: returns an array of 'RepositoryDesc' references
#   containing the repository information
sub getRepos {
  my ($self, $display) = @_;
  return $self->{OpenADRepos};
}

sub initialize {
  my ($self) = @_;
  
  $self->{OpenADRepos} = [ ];
  push(@{$self->{OpenADRepos}}, $OPENAD_OPEN64);
  push(@{$self->{OpenADRepos}}, $OPENAD_OPENADFORTTK);
  push(@{$self->{OpenADRepos}}, $OPENAD_OPENANALYSIS);
  push(@{$self->{OpenADRepos}}, $OPENAD_XERCESC);
  push(@{$self->{OpenADRepos}}, $OPENAD_XAIFBOOSTER);
  push(@{$self->{OpenADRepos}}, $OPENAD_ANGEL);
  push(@{$self->{OpenADRepos}}, $OPENAD_BOOST);
  push(@{$self->{OpenADRepos}}, $OPENAD_XAIF);
}

1;

#############################################################################

# Local Variables:
# perl-indent-level: 2
# End:
