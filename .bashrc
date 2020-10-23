#
# ~/.bashrc
#

shopt -s globstar

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#standard editor variable
export EDITOR=/usr/bin/nvim
export BROWSER="/usr/bin/firefox --new-window"

# Add criteo related utilities
if [ -f "$HOME/.bash_criteo" ] ; then
  . $HOME/.bash_criteo
fi

# modified commands
command -v colordiff >/dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias mkdir='mkdir -p -v'
alias ..='cd ..'

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

which gti >/dev/null 2>&1 || alias gti='git'                     # alias because of frequent typo
alias m='mutt'
alias ssh="TERM=xterm ssh"
alias cp="cp -v -R"

alias dict="cat /usr/share/dict/words"

alias b='bundle install'
alias bb='mv Gemfile.lock .Gemfile.lock.$(date +%s); bundle install'

alias js="mosespa search"

# will try ping until success. useful to wait for network to come back
function ping_until {
  until ping -c 3 -W 1 -q $1 > /dev/null ; do echo -n .; sleep 0.4; done
}

function nc_until {
  until (echo "" | nc -w 1 $1 $2) > /dev/null ; do echo -n .; sleep 0.4; done
}

function ssh_until {
  echo -n ping
  ping_until $1
  echo ""
  echo -n "ssh_port"
  nc_until $1 22
  echo ""
}

function chef_until {
  now=$(date +%s)
  res=0
  while [ "$res" -eq 0 ]; do
    res=$(bundle exec knife search "fqdn:$1 AND NOT ohai_time:[0 TO $now]" 2> /dev/null| wc -l)
    echo -n .
    sleep 2
  done
}




alias irb="pry"

# privileged access
if [ $UID -ne 0 ]; then
  alias sudo='sudo '
  alias suvim='sudoedit'
fi

# ls
alias ls='ls -hF --color=auto'
alias ll='ls -l'
alias la='ll -A'

#use colors
if [ -f ~/.dircolors ]; then
  eval `dircolors ~/.dircolors`
fi

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

if [ -f $HOME/.bash_completion ]; then
    . $HOME/.bash_completion
fi

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
  . /usr/share/bash-completion/bash_completion

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
LAST_COMMAND_TIMER='$(if [[ $timer_show -gt 6 ]]; then echo "${timer_show}s "; fi)'

RUBY_REPO='$(if test -f Gemfile; then echo "💎 ";fi)'

if [ -f ~/.git-prompt.sh ]; then
  . ~/.git-prompt.sh
  export GIT_BRANCH="\$(__git_ps1 \" $UWhite%.3s\")${NORM}"
fi


if [ -n "$SSH_CLIENT" ]; then
  HOST="${CYAN}\h${NORM} "
  USER_="${RED}\u${NORM}"
fi

case $TERM in
  xterm*)
    XTERM_TITLE='\[\033]0;\u@\h:\w\007\]'
    ;;
  rxvt-unicode)
    XTERM_TITLE='\[\033]0;\u@\h:\w\007\]'
    ;;
  *)
    XTERM_TITLE=''
    ;;
esac

export PS1="\t ${XTERM_TITLE}${USER_}${HOST}${YELLOW}\w${NORM}${RUBY_REPO}${GIT_BRANCH} ${LAST_COMMAND_TIMER}$LAST_COMMAND_RESULT $BELL"

# Measure how long commands last
# the result can be called using `echo $timer_show`
function timer_start {
  timer=${timer:-$SECONDS}
}
function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
}
trap 'timer_start' DEBUG # TODO consider using $PS0 (http://stromberg.dnsalias.org/~strombrg/PS0-prompt/)

# called before each prompt
# use it for all dynamic settings
function prompt_command {
 last=$?
 timer_stop
 if [ "$(id -u)" -ne 0 ]; then
   echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.bash-history-$(date "+%Y-%m-%d").log
 fi
}

PROMPT_COMMAND=prompt_command


export PATH=$PATH:~/.dotfiles/scripts/:/usr/bin/vendor_perl:~/.cabal/bin

export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
RUBY_PATH=$GEM_HOME/bin
export PATH="$PATH:$RUBY_PATH"


#Workstation or personnal desktop?
if [ $(uname -n) == "criteo-scalasto" ]; then
  export MAILDIR=$HOME/Mail/Criteo
  export EMAIL="g.seux@criteo.com"
elif [ $(uname -n) == "vargas" ]; then
  export EMAIL="g.seux@criteo.com"
elif [ $(uname -n) == "churchill" ]; then
  export EMAIL="g.seux@criteo.com"
else
  export MAILDIR=$HOME/Maildir
  export EMAIL="kamaradclimber@gmail.com"
fi


if [ -d "$HOME/.local/bin/" ]; then
  export PATH=$PATH:$HOME/.local/bin/
fi

if [ -d "/var/lib/snapd/snap/bin" ]; then
  export PATH=$PATH:/var/lib/snapd/snap/bin
fi

[ -f ~/.bundler-exec.sh ] && source ~/.bundler-exec.sh

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

if test_helper "ag" "the_silver_searcher"; then
  if test_helper "tag" "tag-ag"; then
    tag() { command tag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null; }
    alias ag=tag
  fi
fi

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh

if test_helper "fzf" "fzf"; then
  source /usr/share/fzf/key-bindings.bash
  # see https://github.com/junegunn/fzf/issues/1203 we can reuse this on fzf 0.17.4
  if grep -q 0.17.4 <(fzf --version); then
    source /usr/share/fzf/completion.bash
  fi
fi

# load history from custom files and use fzf
if test -f $HOME/.dotfiles/custom_history.sh; then
  source $HOME/.dotfiles/custom_history.sh
fi

function gotmp() {
  dir=$(mktemp -d)
  cd $dir
}

function my_private_ipaddress() {
  ip addr | grep 'inet ' | awk '{print $2}' | cut -f1 -d'/' | grep -e 192.168 -e ^172 | sort | head -n1
}

function webserver() {
  my_ip=$(my_private_ipaddress)
  pkill python3
  python3 -m http.server 8000 > /dev/null &
  echo "server accessible on http://$my_ip:8000/"
  fg > /dev/null 2>&1
}

if test_helper "bat" "bat"; then
  alias cat='bat --paging=never'
fi

alias idea="_JAVA_AWT_WM_NONREPARENTING=1 idea"

# sadly there are too many warnings when using ruby 2.6
# DONT ENABLE THIS since it blocks all programs that use "warn" in ruby code
export RUBYOPT="-W0"

function yaourt() {
  echo "Use yay instead!"
}

if [ $(tty) = /dev/tty1 ]; then
  startx
  exit 0
fi

# image display with kitty
alias icat="kitty +kitten icat"

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


if test_helper "nvim" "neovim"; then
  alias vim=nvim
fi




if test_helper "grc" "grc"; then
  source /etc/profile.d/grc.bashrc
fi


# Add criteo related utilities
if [ -f "$HOME/.bash_vpn" ] ; then
  . $HOME/.bash_vpn
fi

true # finish with a correct exit code

