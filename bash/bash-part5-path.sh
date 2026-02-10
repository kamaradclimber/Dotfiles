export PATH=$PATH:~/.dotfiles/scripts/

# put rbenv shims in front of system install for ruby
export PATH="/Users/gregoire.seux/.rbenv/shims/:$PATH"

# classical method is to run `ruby -e 'puts Gem.user_dir'` and add the bin directory to the PATH
# however, invocation of ruby costs 100ms on macos, so let's use a cached approach
LIKELY_RUBY_HOME=$HOME/.gem/ruby
if [ -d "$LIKELY_RUBY_HOME" ]; then
  # Use bash globbing instead of ls+sort+tail+sed (much faster)
  # This finds the latest ruby version directory
  shopt -s nullglob
  ruby_dirs=("$LIKELY_RUBY_HOME"/*/bin)
  if [ ${#ruby_dirs[@]} -gt 0 ]; then
    # Get the last one (highest version due to alphabetical sorting)
    latest_ruby_bin="${ruby_dirs[-1]}"
    # Extract just the version number using bash parameter expansion
    RUBY_VERSION="${latest_ruby_bin%/bin}"
    RUBY_VERSION="${RUBY_VERSION##*/}"
    export GEM_HOME="$LIKELY_RUBY_HOME/$RUBY_VERSION"
    export PATH="$PATH:$GEM_HOME/bin"
  fi
  shopt -u nullglob
fi


if [ -d "$HOME/.local/bin/" ]; then
  export PATH=$PATH:$HOME/.local/bin/
fi

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

export PATH="/ssd/home/grego/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/:$PATH"

# we want binaries from homebrew to take precedence over system binaries (this is important for having a recent git for instance)
export PATH=/opt/homebrew/bin:$PATH

if test -d $HOME/.atuin/bin; then
  . "$HOME/.atuin/bin/env"
fi
