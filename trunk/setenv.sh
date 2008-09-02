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
