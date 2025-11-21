export EMAIL="grego_github@familleseux.net"

#standard editor variable
export EDITOR=/usr/bin/vim
if [[ -f /usr/bin/nvim ]]; then
  export EDITOR=/usr/bin/nvim
fi
if [[ -f /home/grego/.local/bin/lvim ]]; then
  export EDITOR=/home/grego/.local/bin/lvim
fi

export BROWSER="$(which firefox) --new-window"


# modified commands
command -v colordiff >/dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias mkdir='mkdir -p -v'
alias ..='cd ..'


which gti >/dev/null 2>&1 || alias gti='git'                     # alias because of frequent typo
alias m='mutt'
alias ssh="TERM=xterm ssh"
alias cp="cp -v -R"

alias dict="cat /usr/share/dict/words"

alias b='bundle install'
alias bb='mv Gemfile.lock .Gemfile.lock.$(date +%s); bundle install'

# privileged access
if [ $UID -ne 0 ]; then
  alias sudo='sudo '
  alias suvim='sudoedit'
fi

# ls
alias ls='ls --hyperlink=auto -hF --color=auto'
alias ll='ls --hyperlink=auto -l'
alias la='ll --hyperlink=auto -A'

#Most used commands
alias muc='cut -f1 -d" " ~/.bash_history | sort | uniq -c | sort -nr | head -n 30'

alias rails="bundle exec rails"

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/ripgrep.conf

if which fdfind > /dev/null 2>&1; then
  # on debian the binary is called fdfind to avoid conflict with another package
  alias fd=fdfind
fi
