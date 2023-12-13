
__fzf_history__() {
  local line
  line=$(
  HISTTIMEFORMAT= command cat ~/.bash_history_storage/* |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac --sync -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m" $(__fzfcmd)) &&
    line=$(awk '{ print substr($0, index($0,$2)) }' <<< "$line")

  READLINE_LINE=${line#*$'\t'}
  if [ -z "$READLINE_POINT" ]; then
    echo "$READLINE_LINE"
  else
    READLINE_POINT=0x7fffffff
  fi
}
