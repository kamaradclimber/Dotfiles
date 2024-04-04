if test_helper "fzf" "fzf"; then
  if test -r /usr/share/fzf/key-bindings.bash; then
    source /usr/share/fzf/key-bindings.bash
  fi
  # see https://github.com/junegunn/fzf/issues/1203 we can reuse this on fzf 0.17.4
  if grep -q 0.17.4 <(fzf --version); then
    source /usr/share/fzf/completion.bash
  fi
  
  if test_helper "ag" "the_silver_searcher"; then
    export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
  fi
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# load history from custom files and use fzf
if test -f $HOME/.custom_history.sh; then
  source $HOME/.custom_history.sh
fi

fzf_git_log() {
  local commits=$(
  git ll --color=always "$@" |
    fzf --ansi --no-sort --height 100% # \
#    --preview "echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |
#    xargs -I@ sh -c 'git show --color=always @'"
  )
  if [[ -n $commits ]]; then
    local hashes=$(printf "$commits" | cut -d' ' -f2 | tr '\n' ' ')
    git show $hashes
  fi
}

alias last='fzf_git_log'


fvim () 
{ 
vim $(ag "$@" | fzf --preview 'bat --color=always {}')
}
