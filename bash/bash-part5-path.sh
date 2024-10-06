export PATH=$PATH:~/.dotfiles/scripts/

# put rbenv shims in front of system install for ruby
export PATH="/Users/gregoire.seux/.rbenv/shims/:$PATH"

# classical method is to run `ruby -e 'puts Gem.user_dir'` and add the bin directory to the PATH
# however, invocation of ruby costs 100ms on macos, so let’s find a better technique
LIKELY_RUBY_HOME=$HOME/.gem/ruby
if [ -d $LIKELY_RUBY_HOME ]; then
  RUBY_VERSION=$(command ls -d $LIKELY_RUBY_HOME/*/bin | sort | tail -n 1 | sed "s|/bin||")
  export GEM_HOME=$RUBY_VERSION
fi
if [[ "$RUBY_VERSION" == "" ]]; then
  export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
fi

RUBY_PATH=$GEM_HOME/bin
export PATH="$PATH:$RUBY_PATH"


if [ -d "$HOME/.local/bin/" ]; then
  export PATH=$PATH:$HOME/.local/bin/
fi

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

export PATH="/ssd/home/grego/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/:$PATH"
export PATH=$PATH:/opt/homebrew/bin

# volta is used to handle nodejs versions
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
