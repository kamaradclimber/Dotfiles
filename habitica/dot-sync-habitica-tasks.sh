#!/usr/bin/bash

HOME=/home/grego
source $HOME/.watson_env

export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
RUBY_PATH=$GEM_HOME/bin
export PATH="$PATH:$RUBY_PATH"

# if the command fails, install https://github.com/kamaradclimber/habitica-tasks
habitica-tasks
