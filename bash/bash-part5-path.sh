export PATH=$PATH:~/.dotfiles/scripts/

# put rbenv shims in front of system install for ruby
export PATH="/Users/gregoire.seux/.rbenv/shims/:$PATH"
export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
RUBY_PATH=$GEM_HOME/bin
export PATH="$PATH:$RUBY_PATH"


if [ -d "$HOME/.local/bin/" ]; then
  export PATH=$PATH:$HOME/.local/bin/
fi

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

export PATH="/ssd/home/grego/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/:$PATH"
export PATH=$PATH:/opt/homebrew/bin
