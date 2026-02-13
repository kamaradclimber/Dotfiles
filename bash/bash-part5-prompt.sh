

# only on macos
if [[ "$(uname -s)" == "Darwin" ]]; then
  if ! test -f /opt/homebrew/bin/starship; then
    echo "Starship prompt not found. Please install it first."
    return 0
  fi
  eval -- "$(/opt/homebrew/bin/starship init bash --print-full-init)"
elif [[ "$(uname -s)" == "Linux" ]]; then
  eval "$(starship init bash)"
fi
