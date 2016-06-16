#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#standard editor variable
export EDITOR=/usr/bin/vim
export BROWSER=/usr/bin/conkeror

# Add criteo related utilities
if [ -f "$HOME/.bash_criteo" ] ; then
  . $HOME/.bash_criteo
fi

# modified commands
command -v colordiff >/dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias mkdir='mkdir -p -v'
alias ..='cd ..'

alias gti='git'                     # alias because of frequent typo
alias m='mutt'
alias ssh="TERM=xterm ssh"
alias cp="cp -v -R"

alias dict="cat /usr/share/dict/words"

alias rdesktop="rdesktop -K -g 1200x800"

# will try ping until success. useful to wait for network to come back
function ping_until {
  until ping -c 3 -W 1 -q $1 > /dev/null ; do echo -n .; sleep 0.4; done
}

function nc_until {
  until nc -w 1 $1 $2 > /dev/null ; do echo -n .; sleep 0.4; done
}

function ssh_until {
  echo -n ping
  ping_until $1
  echo -n ssh_port
  nc_until $1 22
}




alias irb="pry"

# privileged access
if [ $UID -ne 0 ]; then
  alias sudo='sudo '
  alias suvim='sudoedit'
  if [ -n "$SSH_CLIENT" ]; then
    alias halt='echo "No you do not want to do that. Otherwise please use /usr/bin/halt"'
    alias reboot='echo "No you do not want to do that. Otherwise please use /usr/bin/reboot"'
    alias shutdown='echo "No you do not want to do that. Otherwise please use /usr/bin/shutdown"'
    alias poweroff='echo "No you do not want to do that. Otherwise please use /usr/bin/poweroff"'
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

if [ -f $HOME/.bash_completion ]; then
    . $HOME/.bash_completion
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
LAST_COMMAND_RESULT="\$(if [[ \$last == 0 || (\$last == 130 || \$last == 141)]]; then echo \"${GREEN}>\"; else echo \"${RED}\\\$?:\$last>\"; fi)${NORM}"
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

# Measure how long commands last
# the result can be called using `echo $timer_show`
function timer_start {
  timer=${timer:-$SECONDS}
}
function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
}
trap 'timer_start' DEBUG


# called before each prompt
# use it for all dynamic settings
function prompt_command {
 last=$?
 __cached_send_location_to_mqtt
 timer_stop
}

PROMPT_COMMAND=prompt_command

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


if [ -d "$HOME/perl5" ] ; then
  export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:$HOME/perl5";
  export PERL_MB_OPT="--install_base $HOME/perl5";
  export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
  export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB";
  export PATH="$HOME/perl5/bin:$PATH"
fi

if [ -d "/opt/chefdk/bin" ]; then
  export PATH="/opt/chefdk/bin:$PATH"
fi

if [ -d "$HOME/.local/bin/" ]; then
  export PATH=$PATH:$HOME/.local/bin/
fi

[ -f ~/.bundler-exec.sh ] && source ~/.bundler-exec.sh

# Smart cd for criteo projects
# TODO move this to .bash_criteo
_gitlab_clone() {
  project=$1
  repo=$2
  dir=$3
  git clone git@gitlab.criteois.com:$project/$repo.git $dir
}

_gerrit_clone() {
  project=$1
  repo=$2
  dir=$3
  git clone ssh://review.criteois.lan:29418/$project/$repo.git $dir
}

ck() {
  ck_dir=~/cookbooks/$1
  [[ ! -d $ck_dir ]] && _gerrit_clone chef-cookbooks $1 $ck_dir
  cd $ck_dir
  (git remote -v | grep -q gitlab) || git remote add gitlab git@gitlab.criteois.com:chef-cookbooks/$1.git
}
_ck_complete() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=( $(compgen -W "$(command ls ~/cookbooks/)" -- $cur) )
}
complete -o default -F _ck_complete ck
repo() {
  name=$(echo $1 | sed 's/^(chef-repos)?/chef-repos/')
  repo_dir=~/chef-repos/$1
  [[ ! -d $repo_dir ]] && _gerrit_clone chef-repositories $name $repo_dir
  cd $repo_dir
  (git remote -v | grep -q gitlab) || git remote add gitlab git@gitlab.criteois.com:chef-cookbooks/$1.git
}
_repo_complete() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=( $(compgen -W "$(command ls ~/chef-repos/)" -- $cur) )
}
complete -o default -F _repo_complete repo
# end of smart


export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

set -o history

open() {
  command=$(history | tail -2 | head -n 1| awk '{$1=""; print}')
  echo $command
  $EDITOR $($command | awk  'BEGIN {FS=":";} {print $1":"$2}')
}

# added by travis gem
[ -f /home/grego/.travis/travis.sh ] && source /home/grego/.travis/travis.sh

true # finish with a correct exit code
