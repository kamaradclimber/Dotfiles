#!/usr/bin/env bash
#
# a custom switcher
if [[ "$#" -eq "0" ]]; then
  command git switch $(git branch --no-color | tr '*' ' ' | fzf)
else
  command git switch 
fi
