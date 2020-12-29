#!/usr/bin/bash

if ! which stow 2> /dev/null; then
  echo You must install GNU stow
  exit 1
fi

stow --dotfiles bash
stow --dotfiles awesome
