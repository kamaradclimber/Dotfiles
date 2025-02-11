#history management
export HISTIGNORE="&:ls:[bf]g:exit:*halt:*reboot" # ignore bg,fg,exit, ls without arguments  + does not remember of commands starting with spaces
export HISTFILESIZE=100000 #commands in the history file
export HOSTSIZE=10000 #commands remembered by one shell
export HISTCONTROL=ingorespace:erasedups
shopt -s histappend
