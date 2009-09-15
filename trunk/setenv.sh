##########################################################
# This file is part of OpenAD released under the LGPL.   #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the OpenAD distribution             #
##########################################################

./tools/setenv/setenv.py --shell=sh > setenv.tmp~
if [ $? -ne 0 ] 
then 
 echo "Error executing ./tools/setenv/setenv.py --shell=sh > setenv.tmp~"
else 
  source setenv.tmp~
  if [ $? -ne 0 ]
  then
    echo "Error executing source setenv.tmp~"
  else 
    rm -f setenv.tmp~
  fi
fi
