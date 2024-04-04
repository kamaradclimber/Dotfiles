test_helper() {
  binary=$1
  package_name=$2
  which $binary > /dev/null 2>&1
  res=$?
  if [[ "$res" -eq 0 ]]; then
    return 0
  else
    if [[ ! -z "$package_name" ]]; then
      echo "$binary is not present, install it using $package_name"
    fi
    return 1
  fi
}

if test_helper "ag" "the_silver_searcher"; then
  if test_helper "tag" "tag-ag"; then
    tag() {
      export TAG_ALIAS_FILE=/tmp/tag_aliases.$$
      command tag "$@";
      source $TAG_ALIAS_FILE 2>/dev/null;
    }
    alias ag=tag
  fi
fi

if test_helper "viddy" "viddy"; then
  # viddy is a replacement (nearly drop-in) for watch which features:
  # - back in time traveling
  # - search
  alias watch=viddy
fi

if test_helper "bat" "bat"; then
  alias cat='bat --paging=never --style=plain'
fi

if test_helper "nvim" "neovim"; then
  alias vim=nvim
fi

if test_helper "grc" "grc"; then
  if test -f /opt/homebrew/Cellar/grc/1.13_1/.bottle/etc/grc.sh; then
    source /opt/homebrew/Cellar/grc/1.13_1/.bottle/etc/grc.sh
  fi
  if test -f /etc/profile.d/grc.sh; then
    source /etc/profile.d/grc.sh
  fi
fi

if test_helper "cola"; then
  source <(cola completion bash | sed 's/cdt/cola/g')
fi

if test_helper "direnv" "direnv"; then
  eval "$(direnv hook bash)"
fi
