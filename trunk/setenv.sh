# -*-Mode: sh;-*-
# For sh variants

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/setenv.sh,v 1.1 2004-05-21 15:06:11 eraxxon Exp $

#echo "$0 $*"

#############################################################################
## Set options
#############################################################################

#if [ -x /bin/basename ]; then
#    basename="/bin/basename"
#else
#    basename="/usr/bin/basename"
#fi

prog="./tools/setenv/setenv.pl"
shell="--shell=sh"

tmpfile="$prog.tmp.$$"

#############################################################################
## Run the program
#############################################################################

cmd="$prog $shell $*"

#echo "--> $cmd"
$cmd  > $tmpfile #2>&1
if [ $? -ne 0 ] ; then
    echo "Error executing: $cmd > $tmpfile"
    return 1
fi

source $tmpfile
if [ $? -eq 0 ] ; then 
    #echo "* Environment changed successfully *"
    /bin/rm -f $tmpfile
else
    echo "*** Environment not changed successfully (see $tmpfile) ***"
    return 1
fi

