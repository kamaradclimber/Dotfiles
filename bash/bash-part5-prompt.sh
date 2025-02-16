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
# FIXME: for some reason it's hard to make this multiline, I should try to fix this
BELL="\a"
LAST_COMMAND_RESULT="\$(if [[ \$last == 0 || (\$last == 130 || \$last == 141)]]; then echo \"${GREEN}>\"; else echo \"${BELL}${RED}\\\$?:\$last\"; fi)${NORM} "


if [ -f ~/.git-prompt.sh ]; then
  . ~/.git-prompt.sh
  export GIT_PS1_SHOWCONFLICTSTATE=yes
  export GIT_BRANCH="\$(__git_ps1)${NORM}"
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

export PS1="\t ${XTERM_TITLE}${USER_}${HOST}${YELLOW}\w${NORM}${GIT_BRANCH} $LAST_COMMAND_RESULT"

# called before each prompt
# use it for all dynamic settings
function prompt_command {
 last=$?
}

PROMPT_COMMAND=prompt_command
