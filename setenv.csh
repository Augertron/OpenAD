# -*-Mode: sh;-*-
# For csh variants

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/setenv.csh,v 1.1 2004-05-21 15:06:03 eraxxon Exp $

#echo "$0 $*"

#############################################################################
## Set options
#############################################################################

#if ( -x /bin/basename ) then
#    set basename = "/bin/basename"
#else
#    set basename = "/usr/bin/basename"
#endif

set prog = "./tools/setenv/setenv.pl"
set shell = "--shell=csh"

set tmpfile = "$prog.tmp.$$"

#############################################################################
## Run the program
#############################################################################

set cmd = "$prog $shell $*"

#echo "--> $cmd"
$cmd > $tmpfile
if ( $status != 0 ) then
    echo "Error executing: $cmd > $tmpfile"
    exit 1
endif

source $tmpfile
if ( $status == 0 ) then
    #echo "* Environment changed successfully *"
    /bin/rm -f $tmpfile
else
    echo "*** Environment not changed successfully (see $tmpfile) ***"
    exit 1
endif

