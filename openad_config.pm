# -*-Mode: perl;-*-
# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/openad_config.pm,v 1.3 2004-07-16 15:34:49 eraxxon Exp $

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

use base qw(Exporter);
use vars qw(@EXPORT @EXPORT_OK);
#@EXPORT_OK = qw();

# FindBin will return the location of the *calling* script and we can't
# yet assume FindBin::again() is widely available. It should be safe to
# assume caller is located in ./tools/xxx/
my $TheRealBin = abs_path("$RealBin/../..");

#############################################################################
## Subpackage configuration information
#############################################################################

my %OpenADReposDesc = 
    (name      => undef, # name
     path      => undef, # path to repository
     repos     => undef, # if undef, considered 'external'; user must manage
     tag       => undef, # version or tag
     var       => undef, # corresponding environment variable
     );

my %OpenADCVSDesc = (iscvs        => 1,
		     rsh          => undef,
		     root         => undef,
		     );

my %OpenADBKDesc = (isbk          => 1,
		    root          => undef,
		    );

#############################################################################

######################################################################
# Repository information
######################################################################

##################################################
# Rice HiPerSoft CVS Repository
##################################################

my $riceUser = 'anoncvs'; # $ENV{'USER'};

my $OPENAD_REPO_RICECVS = { %OpenADCVSDesc, }; 
$OPENAD_REPO_RICECVS->{rsh} = 
    "$TheRealBin/tools/sshcvs/sshcvs-hipersoft-anon";
$OPENAD_REPO_RICECVS->{root} = 
    ':ext:' . $riceUser . '@koolkat2.cs.rice.edu:/Volumes/cvsrep/developer';

##################################################
# Bkbits Bitkeeper Repositories
##################################################

my $OPENAD_REPO_BK_XAIF = { %OpenADBKDesc, }; 
$OPENAD_REPO_BK_XAIF->{root} = 'http://xaif.bkbits.net/xaif-1.0';

##################################################
# SourceForge CVS Repositories
##################################################

my $OPENAD_REPO_SF_BOOST = { %OpenADCVSDesc, }; 
$OPENAD_REPO_SF_BOOST->{rsh} = 
    'pserver';
$OPENAD_REPO_SF_BOOST->{root} = 
    ':pserver:anonymous@cvs.sourceforge.net:/cvsroot/boost';

##################################################
# Argonne Bitkeeper
##################################################

my $anlUser = $ENV{'USER'};

my $OPENAD_REPO_ANLBK_XB = { %OpenADBKDesc, }; 
$OPENAD_REPO_ANLBK_XB->{root} = $anlUser . 
    '@terra.mcs.anl.gov:/home/utke/bk_tmpRep/xaifBooster_CFR/xaifBooster';

my $OPENAD_REPO_ANLBK_ANGEL = { %OpenADBKDesc, }; 
$OPENAD_REPO_ANLBK_ANGEL->{root} = $anlUser . 
    '@terra.mcs.anl.gov:/home/utke/BK_Reps/CODE/angel';


######################################################################
# OpenAD Repositories, Local Instances
######################################################################  

my $OPENAD_OPEN64 = { %OpenADReposDesc, };
$OPENAD_OPEN64->{name}  = 'Open64';
$OPENAD_OPEN64->{path}  = "$TheRealBin";
$OPENAD_OPEN64->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_BOOST->{tag}    = 'OpenAD';
$OPENAD_OPEN64->{var}   = 'OPEN64_BASE';

my $OPENAD_OPENADFORTTK = { %OpenADReposDesc, };
$OPENAD_OPENADFORTTK->{name}  = 'OpenADFortTk';
$OPENAD_OPENADFORTTK->{path}  = "$TheRealBin";
$OPENAD_OPENADFORTTK->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_OPENADFORTTK->{var}   = 'OPENADFORTTK_BASE';

my $OPENAD_OPENANALYSIS = { %OpenADReposDesc, };
$OPENAD_OPENANALYSIS->{name}  = 'OpenAnalysis';
$OPENAD_OPENANALYSIS->{path}  = "$TheRealBin";
$OPENAD_OPENANALYSIS->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_OPENANALYSIS->{var}   = 'OPENANALYSIS_BASE';

my $OPENAD_XERCESC = { %OpenADReposDesc, };
$OPENAD_XERCESC->{name}  = 'xercesc';
$OPENAD_XERCESC->{path}  = "$TheRealBin";
$OPENAD_XERCESC->{repos} = $OPENAD_REPO_RICECVS;
$OPENAD_XERCESC->{var}   = 'XERCESC_BASE';

my $OPENAD_XAIFBOOSTER = { %OpenADReposDesc, };
$OPENAD_XAIFBOOSTER->{name}  = 'xaifBooster';
$OPENAD_XAIFBOOSTER->{path}  = "$TheRealBin";
$OPENAD_XAIFBOOSTER->{repos} = $OPENAD_REPO_ANLBK_XB;
$OPENAD_XAIFBOOSTER->{var}   = 'XAIFBOOSTER_BASE';

my $OPENAD_ANGEL = { %OpenADReposDesc, };
$OPENAD_ANGEL->{name}  = 'angel';
$OPENAD_ANGEL->{path}  = "$TheRealBin";
$OPENAD_ANGEL->{repos} = $OPENAD_REPO_ANLBK_ANGEL;
$OPENAD_ANGEL->{var}   = 'ANGEL_BASE';

my $OPENAD_BOOST = { %OpenADReposDesc, };
$OPENAD_BOOST->{name}  = 'boost';
$OPENAD_BOOST->{path}  = "$TheRealBin";
$OPENAD_BOOST->{repos} = $OPENAD_REPO_SF_BOOST;
$OPENAD_BOOST->{tag}   = 'Version_1_30_2';
$OPENAD_BOOST->{var}   = 'BOOST_BASE';

my $OPENAD_XAIF = { %OpenADReposDesc, };
$OPENAD_XAIF->{name}  = 'xaif-1.0';
$OPENAD_XAIF->{path}  = "$TheRealBin";
$OPENAD_XAIF->{repos} = $OPENAD_REPO_BK_XAIF;
$OPENAD_XAIF->{var}  = 'XAIFSCHEMA_BASE';

#############################################################################
## Methods
#############################################################################

# A list of all repositories (OpenADReposDesc) in this configuation of OpenAD

sub new {
  my $class = shift;
  my $self = { };
  bless $self, $class;
  $self->initialize();
  return $self;
}  

# getRepos: returns an array reference containing the list of repositories
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
