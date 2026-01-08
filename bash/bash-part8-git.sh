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
  if echo "$f" | grep "^e[0-9]\+$"; then
    g=$(alias $f | cut -d= -f2 | cut -d'"' -f4)
    line=$(alias $f | cut -d= -f2 | sed -re "s/.*call cursor\(([0-9]+),.*/\1/")
    echo "Replacing $f with $g"
    echo "Found line $line"
    f=$g
  fi
  git_root_dir=$(git rev-parse --show-toplevel)
  repo=$(git remote -v | awk '{print $2}' | sed -re "s/.+github.com:?//" | sed -re "s/\.git$//"| head -n 1)
  main_branch=$(git rev-parse --abbrev-ref origin/HEAD | sed 's|origin/||')

  if test -f $f; then
    fqdn_path=$(realpath --relative-to=$git_root_dir $f)
    url=https://github.com/$repo/blob/$main_branch/$fqdn_path
    if [[ ! -z "$line" ]]; then
      url="${url}#L${line}"
    fi
  fi

  # check if f looks like a commit hash
  if [[ $f =~ ^[0-9a-f]{5,40}$ ]]; then
    echo "Looks like a commit hash"
    git show $f > /dev/null 2>&1
    if [[ $? -eq 0 ]]; then
      url=https://github.com/$repo/commit/$f
    fi
  fi

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

find-codeowner() {
  f=$1
  git_root_dir=$(git rev-parse --show-toplevel)
  fqdn_path=$(realpath --relative-to=$git_root_dir $f)
  code_owner_path=$git_root_dir/.github/CODEOWNERS
  if ! test -f $code_owner_path; then
    echo "No CODEOWNERS file found in $code_owner_path"
    return 1
  fi

  owned_path=$fqdn_path
  while true; do
    # echo "Checking for mention of $owned_path"
    if [[ -z $owned_path ]]; then
      echo "No code owner found" >&2
      return 1
    fi
    if [[ $owned_path == "." ]]; then
      echo "No code owner found" >&2
      return 1
    fi
    code_owner=$(grep -e "/$owned_path\s" $code_owner_path | awk '{print $2}')
    if [[ -n $code_owner ]]; then
      echo "$code_owner owns $owned_path"
      return 0
    fi
    owned_path=$(dirname $owned_path)
  done
}

all-my-prs() {
  repo=$(git remote -v | awk '{print $2}' | sed -re "s/.+github.com:?//" | sed -re "s/\.git$//"| head -n 1)
  if [[ -z $repo ]]; then
    echo "❌ Not in a git repository with GitHub remote"
    return 1
  fi
  
  username=$(gh api user --jq '.login')
  if [[ -z $username ]]; then
    echo "❌ Unable to get GitHub username. Make sure gh CLI is authenticated."
    return 1
  fi
  
  url="https://github.com/$repo/pulls?q=is%3Apr+is%3Aopen+author%3A$username"
  echo "Opening PRs for $username in $repo"
  
  if [[ -n $BROWSER ]]; then
    if which $BROWSER 2> /dev/null; then
      $BROWSER "$url" && return
    fi
  fi
  if which xdg-open 2> /dev/null; then
    xdg-open "$url"
  else # macos case
    open "$url"
  fi
}

# push the local branch and create a pr with gh CLI
pr() {
  br=$(git branch --show-current)
  echo "Will create a PR for ${br} branch"
  main_br=$(git rev-parse --abbrev-ref origin/HEAD | sed 's|origin/||')
  if [[ "$br" == "$main_br" ]]; then
    echo "❌ Will not push to the main branch"
    return 1
  fi

  if gh pr create --help | grep -q fill-verbose; then
    git push origin HEAD && gh pr create --fill-verbose --draft --head=$(git branch --show-current)
  else
    git push origin HEAD && gh pr create --fill --draft --head=$(git branch --show-current)
  fi

}
