#Completion
complete -cf sudo
complete -cf pacman
complete -cf man

# Add bash completion for ssh: it tries to complete the host to which you
# want to connect from the list of the ones contained in ~/.ssh/known_hosts

__ssh_known_hosts() {
  if [[ -f ~/.ssh/known_hosts ]]; then
    cut -d " " -f1 ~/.ssh/known_hosts | cut -d "," -f1 |sed 's/\[\|\]\|\(:[0-9]*\)//g'
  fi
  if [[ -f ~/.ssh/config ]]; then
    cat ~/.ssh/config | awk '/Host / {print $2}' | grep -v '*' | xargs -n1
  fi
}

_ssh() {
    local cur known_hosts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    known_hosts="$(__ssh_known_hosts)"

    if [[ ! ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${known_hosts}" -- ${cur}) )
        return 0
    fi
}

complete -o bashdefault -o default -o nospace -F _ssh ssh 2>/dev/null \
    || complete -o default -o nospace -F _ssh ssh



#if present, use bash completion specifics. TODO  : quid of bash_completion.d/ dir ?
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
# Selective bash completion loading
# Instead of loading all 222 completion files (~150ms), only load what we actually use
# Based on atuin stats of most frequently used commands

if [[ -d "/opt/homebrew/etc/bash_completion.d" ]]; then
  # Manually source only the completions we actually use
  # Ordered by frequency: git, kubectl, ag, python, brew, make, npm, aws, gh, helm
  for completion_file in \
    git-completion.bash \
    kubectl \
    ag.bashcomp.sh \
    python \
    brew \
    make \
    npm \
    aws_bash_completer \
    gh \
    helm \
    docker-compose; do
    if [[ -r "/opt/homebrew/etc/bash_completion.d/$completion_file" ]]; then
      source "/opt/homebrew/etc/bash_completion.d/$completion_file" 2>/dev/null
    fi
  done
fi

if [ -f $HOME/.bash_completion ]; then
    . $HOME/.bash_completion
fi

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
  . /usr/share/bash-completion/bash_completion


# by default we complete ag with directory names. It’s better than nothing
complete -o dirnames ag
