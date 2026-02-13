#!/usr/bin/env bash

# This is the script used by dd workspace command

set -euo pipefail

echo "Preparing my workspace"

export IS_DATADOG_WORKSPACE=true

cd $HOME

sudo apt update
sudo apt install stow

ln -sf dotfiles .dotfiles

rm -f ~/.bashrc

cd .dotfiles
for prog in ai-agents nix bash git neovim; do
  echo "Will use stow on $prog"
  stow --dotfiles $prog --verbose 2 --ignore=setup
  if test -f $prog/setup; then
    echo "Running setup script"
    pushd $prog
    ./setup
    popd
  fi
done

