#!/bin/sh

last=""
for l in `find $1 -type d -name cur -printf '%h\n' |sed 's/ /_/g'`; do
  this=$(dirname $l | tr '/' ' ' |awk '{print $NF}')
  if [ "$last" != "$this" ]; then
    echo "=--$this-"
    last=$this
  fi
  echo "$l"
done | tr '\n' ' '
