# ~/.zshrc: Personal settings for zsh

####################
# Personal aliases #
####################

alias bidounet=192.168.1.71
alias adama=88.163.232.134
alias sshb='ssh grego@bidounet'
alias ssha='ssh kamaradclimber@adama'


# Secure SHell access to some machines



######################
# Personal functions #
######################

case $TERM in
xterm*)
precmd () {print -Pn "\e]0;%n@%m: %~\a"}
;;
esac



# Locales {{{
	export LANG=en_US.utf8
		export LANGUAGE=en_US.utf8
		export LC_ADDRESS=en_US.utf8
		export LC_ALL=en_US.utf8
		export LC_COLLATE=en_US.utf8
		export LC_IDENTIFICATION=en_US.utf8
		export LC_MESSAGES=en_US.utf8
		export LC_MEASUREMENT=en_US.utf8
		export LC_MONETARY=en_US.utf8
		export LC_NAME=en_US.utf8
		export LC_NUMERIC=en_US.utf8
		export LC_PAPER=en_US.utf8
		export LC_TELEPHONE=en_US.utf8
		export LC_TIME=en_US.utf8
		export LC_TYPE=en_US.utf8

#enforce azerty
		setxkbmap -layout fr


#unicode_start
# }}}


# Constants {{{
	export COLORTERM="yes"
		export EDITOR="/usr/bin/vim"
		export GREP_COLOR=31
		export HISTSIZE=1000
		export HISTFILE=~/.history_zsh
		export MAIL=${HOME}/mail
		export MAILCHECK=1
		export PATH=.cabal/bin:/usr/share/perl5/vendor_perl/auto/share/dist/Cope:/usr/share/perl5/vendor_perl/auto/share/dist:${PATH}
	export SAVEHIST=1000
		export GDK_USE_XFT=1    #   For old gtk applications
		export QT_XFT=true      #   For old qt applicatios
# }}}


#history, read from online manual
		setopt share_history   # share history in all the zsh console opened at the same time
		setopt inc_append_history 

		set hist_ignore_dups #ignore two consecutive same lines in history




# Keybindings {{{


	bindkey "\e[1~" beginning-of-line # Home
		bindkey "\e[4~" end-of-line # End
		bindkey "\e[5~" beginning-of-history # PageUp
		bindkey "\e[6~" end-of-history # PageDown
		bindkey "\e[2~" quoted-insert # Ins
		bindkey "\e[3~" delete-char # Del
		bindkey "\e[5C" forward-word
		bindkey "\eOc" emacs-forward-word
		bindkey "\e[5D" backward-word
		bindkey "\eOd" emacs-backward-word
		bindkey "\e\e[C" forward-word
		bindkey "\e\e[D" backward-word
# for rxvt
		bindkey "\e[7~" beginning-of-line # Home
		bindkey "\e[8~" end-of-line # End
# for non RH/Debian xterm, can't hurt for RH/Debian xterm
		bindkey "\eOH" beginning-of-line
		bindkey "\eOF" end-of-line
# for freebsd console
		bindkey "\e[H" beginning-of-line
		bindkey "\e[F" end-of-line

# clever search in history file of zsh
		bindkey "^[[A" history-search-backward
		bindkey "^[[B" history-search-forward
# }}}


		autoload -U compinit; compinit

		zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
		zstyle ':completion:*:warnings' format '%BSorry, no result for : %d%b'
		zstyle ':completion:*' menu select=2
		zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

# Remove trailing slashes
		zstyle ':completion:*' squeeze-slashes true

# Use cache
		zstyle ':completion:*' use-cache on
		zstyle ':completion:*' cache-path ~/.zsh_cache

# Prevent CVS files/directories from being completed
		zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
		zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# Allow mistakes
		zstyle ':completion:*' completer _complete _match _approximate
		zstyle ':completion:*:match:*' original only
#zstyle ':completion:*:approximate:*' max-errors 1 numeric
		zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# [?] Ignore completion functions for commands you donâ€™t have
		zstyle ':completion:*:functions' ignored-patterns '_*'

# Colors
# You can also add different colours to the completion list - as displayed in the screenshot below. To be more specific, we'll use the same colours that GNU ls shows with the --color option
		zmodload zsh/complist
		zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Do not show already selected elements
		zstyle ':completion:*:rm:*' ignore-line yes
		zstyle ':completion:*:mv:*' ignore-line yes
		zstyle ':completion:*:cp:*' ignore-line yes

#   Sudo completion
		zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
			/usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# PID completion
			zstyle ':completion:*:*:kill:*:processes'   list-colors "=(#b) #([0-9]#)*=36=31"
			zstyle ':completion:*:*:kill:*'             menu yes select
			zstyle ':completion:*:kill:*'               force-list always
			zstyle ':completion:*:*:killall:*'          menu yes select
			zstyle ':completion:*:killall:*'            force-list always
			zstyle ':completion:*:processes'            command "ps -au$USER"

# Don't select parent directory on cd
			zstyle ':completion:*:cd:*' ignore-parents parent pwd

# xdvi completion
			zstyle ':completion:*:*:xdvi:*' menu yes select
			zstyle ':completion:*:*:xdvi:*' file-sort time

#   Support des fonctions de complÃ©tion de bash
			autoload -U bashcompinit; bashcompinit
# }}}

# Prompts {{{


	autoload -Uz vcs_info

		zstyle ':vcs_info:*' stagedstr '%F{28}●'
		zstyle ':vcs_info:*' unstagedstr '%F{11}●'
		zstyle ':vcs_info:*' check-for-changes true
		zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
		zstyle ':vcs_info:*' enable git svn
		precmd () {
			if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
				zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}]'
			} else {
				zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{blue}]'
			}

			vcs_info
		}
	setopt prompt_subst
		PROMPT='%F{blue}%n@%m %c${vcs_info_msg_0_}%F{blue} %(?/%F{blue}/%F{red})%% %{$res'



# Enable preconfigured prompts
#autoload -U promptinit
#promptinit
#prompt adam2

			BLACK="%{"$'\033[01;30m'"%}"
				RED="%{"$'\033[01;31m'"%}"
				GREEN="%{"$'\033[01;32m'"%}"
				YELLOW="%{"$'\033[01;33m'"%}"
				BLUE="%{"$'\033[01;34m'"%}"
				MAGENTA="%{"$'\033[01;35m'"%}"
				CYAN="%{"$'\033[01;36m'"%}"
				BOLD="%{"$'\033[01;39m'"%}"
				NORM="%{"$'\033[00m'"%}"

				export PS1="[%T] ${RED}%n${NORM}@${BLUE}%m${NORM}:${YELLOW}%~${NORM} %#>"
# }}}

# Aliases {{{
	alias du='du -chs'
		alias egrep='egrep --color=auto'
		alias fgrep='fgrep --color=auto'
		alias grep='grep --color=auto'
		alias la='ls -a'
		alias ll='ls -l'
		alias lla='ls -la'
		alias ls='ls --tabsize=0 --literal --color=auto --show-control-chars --human-readable --group-directories-first -X'
		alias mkdir='mkdir -p -v'
		alias mv='mv -v'
		alias o='xdg-open'
		alias pingg='ping www.google.fr'
		alias suvim='sudo vim'
		alias volume='alsamixer -c 0'
		alias wtf='dmesg'
		alias rtfm='man'
		alias e='vim'

#archlinux some package needed
		alias pacman='sudo pacman -color'

		if [ $UID -ne 0 ]; then
			alias reboot='sudo reboot'
				alias halt='sudo halt'
				fi

# File extension => application
				alias -s tex=vim
				alias -s pdf=epdfview

		alias gobepo='setxkbmap fr bepo'
		alias gofr='setxkbmap fr'

# Options {{{
	setopt auto_cd  
#setopt correctall              #   Correction of writing errors (ex: sl => ls)
		setopt autopushd pushdminus pushdsilent pushdtohome
		setopt cdablevars
#setopt ignoreeof               #   Prevent from using Ctrl + d
		setopt interactivecomments
		setopt noclobber
		setopt SH_WORD_SPLIT
		setopt nohup
		setopt chase_links              #   Traite les liens symboliques comme il faut

#   Quand l'utilisateur commence sa commande par '!' pour faire de la
#   complÃ©tion historique, il n'exÃ©cute pas la commande immÃ©diatement
#   mais il Ã©crit la commande dans le prompt
		setopt hist_verify

		setopt extendedglob             #   Use regexp in commands (ex : cp ^*.(tar|bz2|gz))

#   No beeps at all !
		unsetopt beep
		unsetopt hist_beep
		unsetopt list_beep

#   Si on utilise des jokers dans une liste d'arguments, retire les jokers
#   qui ne correspondent Ã  rien au lieu de donner une erreur
		setopt nullglob



		autoload run-help


#   Inline completion
		set always_to_end
# }}}

#At END of the file : source the zsh-highlighting file
source ~/code/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
