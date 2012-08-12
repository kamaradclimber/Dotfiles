#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#standard editor variable
export EDITOR=vim
export BROWSER=chromium


# modified commands
alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias ..='cd ..'
alias top='htop'
alias gti='git'                     # alias because of frequent typo
alias m='mutt'
alias ssh="TERM=xterm ssh"
alias cp="cp -v -R"

#git shortcuts
alias conflicts="git ls-files --unmerged | cut -f2 | uniq"


# privileged access
if [ $UID -ne 0 ]; then
    alias sudo='sudo '
    alias suvim='sudo vim'
    alias reboot='sudo reboot'
    alias halt='sudo halt'
fi

# ls
alias ls='ls -hF --color=auto'
alias lr='ls -R'                    # recursive ls
alias ll='ls -l'
alias la='ll -A'
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date
alias lm='la | more'


#As a reminder of common commands for pacman, let this aliases here even if unused
# pacman aliases (if applicable, replace 'pacman' with 'yaourt'/'pacaur'/whatever)
alias pacman='pacmatic'
alias pac="pacman -S"      # default action     - install one or more packages
alias pacu="pacman -Syu"   # '[u]pdate'         - upgrade all packages to their newest version
alias pacs="pacman -Ss"    # '[s]earch'         - search for a package using one or more keywords
alias paci="pacman -Si"    # '[i]nfo'           - show information about a package
alias pacr="pacman -R"     # '[r]emove'         - uninstall one or more packages
alias pacl="pacman -Sl"    # '[l]ist'           - list all packages of a repository
alias pacll="pacman -Qqm"  # '[l]ist [l]ocal'   - list all packages which were locally installed (e.g. AUR packages)
alias paclo="pacman -Qdt"  # '[l]ist [o]rphans' - list all packages which are orphaned
alias paco="pacman -Qo"    # '[o]wner'          - determine which package owns a given file
alias pacf="pacman -Ql"    # '[f]iles'          - list all files installed by a given package
alias pacc="pacman -Sc"    # '[c]lean cache'    - delete all not currently installed package files
alias pacm="makepkg -fci"  # '[m]ake'           - make package from PKGBUILD file in current directory



#Completion
complete -cf sudo
complete -cf pacman
complete -cf man

# Add bash completion for ssh: it tries to complete the host to which you 
# want to connect from the list of the ones contained in ~/.ssh/known_hosts

__ssh_known_hosts() {
    if [[ -f ~/.ssh/known_hosts ]]; then
        cut -d " " -f1 ~/.ssh/known_hosts | cut -d "," -f1
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

#colored reading of log files
logview() 
{
    ccze -A < $1 | less -R
}

#same colored reading with tail
logtail() 
{
    tail -f $1 | ccze
}


man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}


#history management
export HISTIGNORE="&:ls:[bf]g:exit:*halt:*reboot" # ignore bg,fg,exit, ls without arguments  + does not remember of commands starting with spaces
export HISTCONTROL=ingorespace:erasedups



shopt -s checkwinsize #allegedly : support of redimensionnable terminals like xterm and screen
shopt -s autocd # allow to cd without typing cd :-)
shopt -s cdspell # minor mistake for cd are corrected
shopt -s no_empty_cmd_completion #no tab mistake
shopt -s histverify #history expansion is displayed before execution


BLACK="\[\e[01;30m\]"
RED="\[\e[01;31m\]"
GREEN="\[\e[01;32m\]"
YELLOW="\[\e[01;33m\]"
BLUE="\[\e[01;34m\]"
MAGENTA="\[\e[01;35m\]"
CYAN="\[\e[01;36m\]"
BOLD="\[\e[01;39m\]"
NORM="\[\e[00m\]"

if [ -n "$SSH_CLIENT" ]; then
    export PS1="[\t] ${RED}\u${NORM}${CYAN}\h${NORM}:${YELLOW}\w${NORM} >"
  else
    export PS1="[\t] ${RED}\u${NORM}${BLUE}\h${NORM}:${YELLOW}\w${NORM} >"
fi

export PATH=~/.gem/ruby/1.9.1/bin:$PATH


###
###     Handy Extract Program
###     found at http://dotfiles.org/~blackbook/.bashrc
###     added 2008-10-07
extract () {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xvjf $1    ;;
             *.tar.gz)    tar xvzf $1    ;;
             *.bz2)       bunzip2 $1     ;;
             *.rar)       unrar x $1     ;;
             *.gz)        gunzip $1      ;;
             *.tar)       tar xvf $1     ;;
             *.tbz2)      tar xvjf $1    ;;
             *.tgz)       tar xvzf $1    ;;
             *.zip)       unzip $1       ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x $1        ;;
             *)           echo "'$1' cannot be extracted via >extract<" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}
