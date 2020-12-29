#!/usr/bin/bash

if ! which stow > /dev/null 2>&1; then
  echo You must install GNU stow
  exit 1
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

find . -maxdepth 1 -mindepth 1 -type d -regex "\./[^.].*" |
  grep -v -e misc$ |
  while read prog; do
  prog=$(echo $prog | cut -f2 -d/)
  echo "Will use stow on $prog"
  stow --dotfiles $prog --verbose 2 --ignore=setup
  if test -f $prog/setup; then
    echo "Running setup script"
    pushd $prog
    ./setup
    popd
  fi
done
