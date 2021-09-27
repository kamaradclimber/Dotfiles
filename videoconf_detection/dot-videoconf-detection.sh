#!/usr/bin/bash

HOME=/home/grego

export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
RUBY_PATH=$GEM_HOME/bin
export PATH="$PATH:$RUBY_PATH"

cd $HOME/home-assistant-video-conf-detection
source my_env
bundle exec bin/home-assistant-video-conf-detection
