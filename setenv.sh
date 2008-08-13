./tools/setenv/setenv.py --shell=sh > setenv.tmp~
if [ $? -ne 0 ] 
then 
  print -u2 "Error executing ./tools/setenv/setenv.py --shell=sh > setenv.tmp~"
else 
  source setenv.tmp~
  if [ $? -ne 0 ]
  then
    print -u2 "Error executing source setenv.tmp~"
  fi
  rm -f setenv.tmp~
fi
