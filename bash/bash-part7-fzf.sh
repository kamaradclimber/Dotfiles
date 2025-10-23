if test_helper "fzf" "fzf"; then
  if test -r /usr/share/fzf/key-bindings.bash; then
    source /usr/share/fzf/key-bindings.bash
  fi
  # see https://github.com/junegunn/fzf/issues/1203 we can reuse this on fzf 0.17.4
  if grep -q 0.17.4 <(fzf --version); then
    source /usr/share/fzf/completion.bash
  fi
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

__fzf_history__() {
  local line
  export HISTTIMEFORMAT=
  export FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac --sync -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"

  line=$(
  for f in ~/.bash_history_storage/*.log; do
    command cat $f
  done | grep -a -v -e "atlas task-queue use" -e "atlas taskqueue use" | $(__fzfcmd)) &&
    line=$(awk '{ print substr($0, index($0,$2)) }' <<< "$line")

  READLINE_LINE=${line#*$'\t'}
  if [ -z "$READLINE_POINT" ]; then
    echo "$READLINE_LINE"
  else
    READLINE_POINT=0x7fffffff
  fi
}

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
