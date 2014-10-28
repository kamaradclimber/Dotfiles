#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#standard editor variable
export EDITOR=/usr/bin/vim
export BROWSER=/usr/bin/conkeror


# modified commands
command -v colordiff >/dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias mkdir='mkdir -p -v'
alias ..='cd ..'

alias gti='git'                     # alias because of frequent typo
alias m='mutt'
alias ssh="TERM=xterm ssh"
alias cp="cp -v -R"

# will try ping until success. useful to wait for network to come back
function ping_until { until [[  $(ping -c 3 -W 1 -q $1 > /dev/null; echo $?) -eq 0 ]]; do echo -n .; sleep 0.4; done }




alias irb="pry"

#probably most used command ever :-)
function psgrep() { ps aux | grep -v grep | grep "$@" -i --color=auto; }
alias pg='psgrep'


# privileged access
if [ $UID -ne 0 ]; then
  alias sudo='sudo '
  alias suvim='sudoedit'
  if [ -n "$SSH_CLIENT" ]; then
    alias halt='echo "No you do not want to do that. Otherwise please use /usr/bin/halt'
    alias reboot='echo "No you do not want to do that. Otherwise please use /usr/bin/reboot'
    alias shutdown='echo "No you do not want to do that. Otherwise please use /usr/bin/shutdown'
    alias poweroff='echo "No you do not want to do that. Otherwise please use /usr/bin/poweroff'
  else
    alias reboot='sudo reboot'
    alias halt='sudo halt'
    alias poweroff='sudo poweroff'
  fi
fi

# ls
alias ls='ls -hF --color=auto'
alias ll='ls -l'
alias la='ll -A'

#use colors
if [ -f ~/.dircolors ]; then
  eval `dircolors ~/.dircolors`
fi

#Archlinux specific
alias pacman='pacmatic'

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

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
  . /usr/share/bash-completion/bash_completion

comp=$(ls $GEM_HOME/gems/mosespa-*/bin/completion_mosespa 2> /dev/null)
[[ $PS1 && -f $comp ]] && \
  source $comp

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
export HISTFILESIZE=100000 #commands in the history file
export HOSTSIZE=10000 #commands remembered by one shell
export HISTCONTROL=ingorespace:erasedups
shopt -s histappend
#Most used commands
alias muc='cut -f1 -d" " ~/.bash_history | sort | uniq -c | sort -nr | head -n 30'


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
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
UWhite="\[\e[4;37m\]"
NORM="\[\e[00m\]"

HOST=""
USER_=""
LAST_COMMAND_RESULT="\$(last=\$?; if [[ \$last == 0 || \$last == 130 ]]; then echo \"${GREEN}>\"; else echo \"${RED}\\\$?:\$last>\"; fi)${NORM}"
BELL="\[\a\]"

if [ -f ~/.git-prompt.sh ]; then
  . ~/.git-prompt.sh
  export GIT_BRANCH="\$(__git_ps1 \" $UWhite%.3s\")${NORM}"
fi


if [ -n "$SSH_CLIENT" ]; then
  HOST="${CYAN}\h${NORM} "
  USER_="${RED}\u${NORM}"
fi
export PS1="\t ${USER_}${HOST}${YELLOW}\w${NORM}${GIT_BRANCH} $LAST_COMMAND_RESULT $BELL"

export PATH=~/.dotfiles/scripts/:/usr/bin/vendor_perl:~/.cabal/bin:$PATH

export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
RUBY_PATH=$GEM_HOME/bin
export PATH="$RUBY_PATH:$PATH"


#Work station or persannal desktop?
if [ $(uname -n) == "criteo-scalasto" ]; then
  export MAILDIR=$HOME/Mail/Criteo
  export EMAIL="g.seux@criteo.com"
elif [ $(uname -n) == "vargas" ]; then
  export EMAIL="g.seux@criteo.com"
else
  export MAILDIR=$HOME/Mail/Gmail
  export EMAIL="kamaradclimber@gmail.com"
fi


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

if [ -d "$HOME/perl5" ] ; then
  export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:$HOME/perl5";
  export PERL_MB_OPT="--install_base $HOME/perl5";
  export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
  export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB";
  export PATH="$HOME/perl5/bin:$PATH"
fi

if [ -f "$HOME/.bash_criteo" ] ; then
  . $HOME/.bash_criteo
fi
[ -f ~/.bundler-exec.sh ] && source ~/.bundler-exec.sh

which rbenv > /dev/null && eval "$(rbenv init -)"
true
