# -*-Mode: sh;-*-
# For sh variants

# $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/OpenAD/setenv.sh,v 1.2 2004-06-08 14:35:31 eraxxon Exp $

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

#############################################################################
## Run the program
#############################################################################

cmd="$prog $shell $*"

#echo "--> $cmd"
eval `$cmd`
if [ $? -ne 0 ] ; then
    echo "*** Error eval'ing: '$cmd' ***"
    return 1
fi
unset cmd
