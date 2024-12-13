# Add work related utilities
if [ -f "$HOME/.bash_work" ] ; then
  . $HOME/.bash_work
fi

export PATH="$HOME/.ddcoterm/bin:$PATH"
export PATH="$HOME/.ddcoterm/overrides:$PATH"

[ -f ~/.config/gitsign/include.sh ] && source ~/.config/gitsign/include.sh
