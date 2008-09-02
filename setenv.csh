#!/bin/csh

./tools/setenv/setenv.py --shell=csh > setenv.tmp~
if ( $status != 0 ) then
  echo  "Error executing: ./tools/setenv/setenv.py --shell=csh > setenv.tmp~"
else
  source setenv.tmp~
  if ( $status != 0 ) then
    echo "Error executing: source setenv.tmp~"
  else
    rm -f setenv.tmp~
  endif
endif
