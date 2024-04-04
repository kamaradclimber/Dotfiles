#history management
export HISTIGNORE="&:ls:[bf]g:exit:*halt:*reboot" # ignore bg,fg,exit, ls without arguments  + does not remember of commands starting with spaces
export HISTFILESIZE=100000 #commands in the history file
export HOSTSIZE=10000 #commands remembered by one shell
export HISTCONTROL=ingorespace:erasedups
shopt -s histappend

mkdir -p .bash_history_storage
find . -maxdepth 1 -name ".bash-history-*.log" | while read file; do
  echo One shot move of $file
  new_name=$(echo $file | awk  -F "/" '{print $NF}' | sed 's/.bash-history-//')
  mv $file .bash_history_storage/$new_name
done
