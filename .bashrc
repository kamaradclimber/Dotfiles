#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#standard editor variable
export EDITOR=vim


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
alias ssh="TERM=linux ssh"
alias cp="vcp -I -R"

#git shortcuts
alias conflicts="git ls-files --unmerged | cut -f2 | uniq"


# privileged access
if [ $UID -ne 0 ]; then
    alias sudo='sudo '
    alias suvim='sudo vim'
    alias reboot='sudo reboot'
    alias halt='sudo halt'
    alias update='sudo pacman -Su'
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
complete -W "`awk '{ print $2 }' /etc/hosts`" ssh

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

__vcs_dir() {
    local vcs base_dir sub_dir ref
    sub_dir() {
        local sub_dir
        sub_dir=$(readlink -f "${PWD}")
        sub_dir=${sub_dir#$1}
        echo ${sub_dir#/}
    }
    # git
    git_dir() {
        base_dir=$(git rev-parse --show-cdup 2>/dev/null) || return 1
        if [ -n "$base_dir" ]; then
            base_dir=`cd $base_dir; pwd`
        else
            base_dir=$PWD
        fi
        sub_dir=$(git rev-parse --show-prefix)
        sub_dir="/${sub_dir%/}"
        ref=$(git symbolic-ref -q HEAD || git name-rev --name-only HEAD 2>/dev/null)
        ref=${ref#refs/heads/}
        vcs="git"
    }
    git_dir || base_dir="$PWD"
    echo "${vcs:+($vcs)}${base_dir/$HOME/~}${vcs:+[$ref]${sub_dir}${_normal}$extra}"
}

BLACK="\[\e[01;30m\]"
RED="\[\e[01;31m\]"
GREEN="\[\e[01;32m\]"
YELLOW="\[\e[01;33m\]"
BLUE="\[\e[01;34m\]"
MAGENTA="\[\e[01;35m\]"
CYAN="\[\e[01;36m\]"
BOLD="\[\e[01;39m\]"
NORM="\[\e[00m\]"

export PS1="[\t] ${RED}\u${NORM}${BLUE}\h${NORM}:${YELLOW}\w${NORM} >"
#export PS1="[\t] ${RED}\u${NORM}${BLUE}\h${NORM}:${YELLOW}${__vcs_dir}${NORM} >"

cp_p()
{
   strace -q -ewrite cp -- "${1}" "${2}" 2>&1 \
      | awk '{
        count += $NF
            if (count % 10 == 0) {
               percent = count / total_size * 100
               printf "%3d%% [", percent
               for (i=0;i<=percent;i++)
                  printf "="
               printf ">"
               for (i=percent;i<100;i++)
                  printf " "
               printf "]\r"
            }
         }
         END { print "" }' total_size=$(stat -c '%s' "${1}") count=0
}
