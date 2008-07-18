./tools/setenv/setenv.py --shell=sh > setenv.tmp~
if [ $? -ne 0 ] 
then 
  print -u2 "Error executing ./tools/setenv/setenv.py --shell=sh > setenv.tmp~"
  exit -1
fi
source setenv.tmp~
if [ $? -ne 0 ]
then
  print -u2 "Error executing source setenv.tmp~"
  exit -1
fi
rm -f setenv.tmp~
