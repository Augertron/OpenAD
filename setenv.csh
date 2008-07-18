#!/bin/csh

./tools/setenv/setenv.py --shell=csh > setenv.tmp~
if ( $status != 0 ) then
  echo  "Error executing: ./tools/setenv/setenv.py --shell=csh > setenv.tmp~"
  exit -1
endif
source setenv.tmp~
if ( $status != 0 ) then
  echo "Error executing: source setenv.tmp~"
  exit -1
endif
rm -f setenv.tmp~
