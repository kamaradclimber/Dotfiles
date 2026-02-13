# fzf setup - loading handled by ~/.fzf.bash
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Note: History search (Ctrl+R) is handled by atuin
# Custom __fzf_history__ function removed as it's unused

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
