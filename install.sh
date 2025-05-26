#!/usr/bin/env bash

# This is the script used by dd workspace command

set -euo pipefail

echo "Preparing my workspace"

cd $HOME

sudo apt update
sudo apt install stow

ln -sf Dotfiles .dotfiles

rm -f ~/.bashrc

cd .dotfiles
for prog in bash git; do
  echo "Will use stow on $prog"
  stow --dotfiles $prog --verbose 2 --ignore=setup
  if test -f $prog/setup; then
    echo "Running setup script"
    pushd $prog
    ./setup
    popd
  fi
done
