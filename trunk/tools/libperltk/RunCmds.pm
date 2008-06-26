# -*-Mode: perl;-*-

# $Header: /Volumes/cvsrep/developer/shared/libperltk/RunCmds.pm,v 1.8 2005/03/08 17:57:22 eraxxon Exp $
## * BeginCopyright *********************************************************
## 
## 
## *********************************************************** EndCopyright *

#############################################################################
##
## $Source: /Volumes/cvsrep/developer/shared/libperltk/RunCmds.pm,v $ 
##
##   Nathan Tallent.
##
#############################################################################

package RunCmds;

use strict;
use warnings;

use Cwd;
use IO::File;

use base qw(Exporter);
use vars qw($VERSION @EXPORT @EXPORT_OK);
$VERSION = '0.001';
@EXPORT_OK = qw(%CmdDesc RunCmds RunCmd);

STDOUT->autoflush(1); 

#############################################################################

# CmdDesc: a shell command and associated metadata
%RunCmds::CmdDesc = 
    (cmd          => undef,
     desc         => undef,
     );

# Note: initial thoughts on abstracting this functionality into
#   something more make-like (where a Makefile could be generated and
#   passed back to the caller; or also executed.).
# target       => undef, # 
# prereqs      => undef, # 
# cmds         => undef, # list of CmdDesc

#############################################################################

# cmdDescVecRef: array of CmdDesc
# verbose: 0 (no), 1 (moderate) or 2 (extreme)
# interactive: 0 or 1
# logfnm: if defined, the name of a log file to which all output should be sent
#
# Note: For now we allow RunCmd to exit on an error.

sub RunCmds
{
  my($cmdDescVecRef, $verbose, $interactive, $logfnm) = @_;
 
  for my $desc (@{$cmdDescVecRef}) {
    if ($verbose >= 1) {
      print STDOUT $desc->{desc}, "\n";
    }
    if ($interactive) {
      print STDOUT "Execute? '$desc->{cmd}'\n";
      print STDOUT "...[Y/n]\n";
      my $ans = <STDIN>;
      chomp($ans);
      if (defined($ans) && ($ans eq 'n' || $ans eq 'N')) {
	last;
      }
    }
    RunCmd($desc->{cmd}, $verbose, $logfnm); 
  }
}

sub RunCmd
{
  my($cmd, $verbose, $logfnm) = @_;
  
  my $dir = cwd();
  my $outfnm = "${dir}/RunCmd-out.tmp~";
  
  $cmd .= " > ${outfnm} 2>&1";
  
  if (defined($logfnm)) {
    # Append separator and command to log file
    my $logfh = new IO::File;
    $logfh->open(">> ${logfnm}") 
	or die "Error! Can't open '${logfnm}'\n";
    print $logfh "*****************************************************************************\n\n";
    print $logfh "${cmd}\n";
    $logfh->close();
  } 
  
  if ($verbose >= 2) {
    print STDOUT "Executing '${cmd}'\n";
  }
  
  if (system($cmd) != 0) {
    my $exit_value  = $? >> 8;
    my $signal_num  = $? & 127;
    my $dumped_core = $? & 128;
    if (defined($logfnm)) { AppendOutfileToLogfile($logfnm, $outfnm); }
    print STDERR "*** Fatal error (exit: $exit_value) ***\n";
    print STDERR "$cmd\n";
    system("cat ${outfnm}");
    exit(-1);
  }

  if (defined($logfnm)) { AppendOutfileToLogfile($logfnm, $outfnm); }
  unlink($outfnm);
}


sub AppendOutfileToLogfile()
{
  my($logfnm, $outfnm) = @_;
  system("cat ${outfnm} >> ${logfnm}");
}


#############################################################################

# Local Variables:
# perl-indent-level: 2
# End:
