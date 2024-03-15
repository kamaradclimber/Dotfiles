#!/usr/bin/env bash
#
# git-files.sh List files that have been touched by a commit
# Usage: git-files.sh <commit>
# if no commit is provided, HEAD is used
#
# goal is to be able to act on the files that have been touched by a commit, for instance edit them


commit=${1:-HEAD}

git show $commit --name-only --pretty="format:" | sort | uniq
