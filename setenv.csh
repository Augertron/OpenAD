# -*-Mode: sh;-*-
# For csh variants

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/setenv.csh,v 1.2 2004-06-08 14:35:31 eraxxon Exp $

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

#############################################################################
## Run the program
#############################################################################

set cmd = "$prog $shell $*"

#echo "--> $cmd"
eval `$cmd`
if ( $status != 0 ) then
    echo "*** Error eval'ing: '$cmd' ***"
    exit 1
endif
unset cmd
