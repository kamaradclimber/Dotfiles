#!/usr/bin/env bash


# this script should be invoked by `git ddco` (see .gitconfig)
# its goal is to build a branch name based on the ticket number

me=gregoire.seux
suggested_branch=$1
# test if suggested_branch is empty
if [[ -z "$suggested_branch" ]]; then
  main_branch=$(git rev-parse --abbrev-ref origin/HEAD | sed 's|origin/||')
  if git branch --show-current | grep -q "$me/\(.\+\)/"; then

    ticket=$(git branch --show-current | sed -E "s|$me/(.+)/.*|\1|")
    echo "Reusing $ticket as ticket name"
  else
    $(which echo) -n "Ticket: "
    read ticket
  fi
  $(which echo) -n "Name: "
  read name
  if [[ ! -z "$name" ]]; then
    if [[ ! -z "$ticket" ]]; then
      name="/$name"
    fi
  fi
  suggested_branch="$ticket$name"
fi
git checkout -b $me/$suggested_branch
