_github_clone() {
  org=$1
  repo=$2
  dir=$3
  echo git clone ssh://git@github.com/$org/$repo.git $dir
  git clone ssh://git@github.com/$org/$repo.git $dir
}

github() {
  local org=$(dirname $1)
  local repo=$(basename $1)
  mkdir -p "$HOME/github/$org"
  dst="$HOME/github/$1"
  old_dst="$HOME/criteo/$1"
  if [[ -d "$old_dst" ]]; then
    mv "$old_dst" "$dst"
  fi
  [[ ! -d "$dst" ]] && _github_clone "$org" "$repo" "$dst"
  cd "$dst"
}
_github_complete() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=( $(compgen -W "$((command find ~/github/ -mindepth 2 -maxdepth 2 -type d) | sed s@//@/@g | sed s@$HOME/github/@@)" -- $cur) )
}
complete -o default -F _github_complete github

open-in-github() {
  f=$1
  git_root_dir=$(git rev-parse --show-toplevel)
  fqdn_path=$(realpath --relative-to=$git_root_dir $f)
  repo=$(git remote -v | awk '{print $2}' | sed -re "s/.+github.com://" | sed -re "s/\.git$//"| head -n 1)
  main_branch=$(git rev-parse --abbrev-ref origin/HEAD | sed 's|origin/||')
  url=https://github.com/$repo/blob/$main_branch/$fqdn_path
  echo "Opening $url"
  if [[ -n $BROWSER ]]; then
    if which $BROWSER 2> /dev/null; then
      $BROWSER $url && return
    fi
  fi
  if which xdg-open 2> /dev/null; then
    xdg-open $url
  else # macos case
    open $url
  fi
}
