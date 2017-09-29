#!/usr/bin/env zsh

### Variables ###
export N_PREFIX=~/.n
export ZSH=~/.oh-my-zsh
export EDITOR=nvim
export PORT=8080
export TERM=xterm-256color


### Aliases ###
alias sudo='sudo '
alias D='cd ~/Desktop'
alias empty='empty-trash'
alias todo='task'
alias :q='exit'
alias :qa='tmux kill-session'
alias t='tmux'
alias ch='git checkout'
alias b='git branch --verbose'
alias c='git commit'


### Functions ###
function mkcd {
  mkdir -p "$1"
  cd "$1" || return 1
}

function edit {
  local session_name="$(basename "$PWD")"
  tmuxinator start edit -n "$session_name"
}

function s {
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo "Not a git repo."
    return 1
  fi

  git status
}

function tdd {
  local utils="$(dotfiles dir)"/utils
  local script="$("$utils"/get-package-test-script.js)"

  if [[ "$?" == 0 ]]; then
    echo "$script" | xargs yarn
  fi
}

function gag {
  ag "$@" \
    --ignore node_modules \
    --ignore '*.bundle.*' \
    --ignore schema.js \
    --ignore dist \
    --ignore coverage \
    --ignore '*.js.map'
}

function v {
  if [[ "$#" == "0" ]]; then
    nvim .
  else
    nvim -p "$@"
  fi
}

function vs {
  nvim -p $(gag "$@" -l)
}

function vp {
  local file_name="${1/.js/}"
  local test_file="__tests__/$file_name"

  if [[ -f "$test_file.spec.js" ]]; then
    test_file="$test_file.spec.js"
  elif [[ -f "$test_file.test.js" ]]; then
    test_file="__tests__/$file_name.test.js"
  else
    test_file="."
  fi

  v "$file_name.js" "$test_file"
}

function _find_file_in_repo {
  local result="$(git ls-files | grep -i $1 | head -1)"

  if [[ -z "$result" ]]; then
    return 1
  fi

  echo "$result"
}

function goto {
  local result="$(_find_file_in_repo "$1")"

  if [[ -z "$result" ]]; then
    echo "Nothing matched."
    return 1
  fi

  cd "$(dirname "$result")" || return 1
}

function gv {
  local result="$(_find_file_in_repo "$1")"

  if [[ -z "$result" ]]; then
    echo "File not found, cap'n."
    return 1
  fi

  cd "$(dirname "$result")"
  nvim "$(basename "$result")"
}

function gvp {
  local file="$(_find_file_in_repo "$1")"

  if [[ -z "$file" ]]; then
    echo "Nope, nothing found."
    return 1
  fi

  cd "$(dirname "$file")"
  vp "$(basename "$file")"
}

function f {
  if [[ -z "$1" ]]; then
    echo "You looking for something?"
    return 1
  fi

  local dir="$2"

  if [[ -z "$dir" ]]; then
    dir="."
  fi

  find "$dir" -iname "*$1*" \
    -not -path '*/node_modules/*' \
    -not -path '*/.git/*' \
    -not -path '*/dist/*' \
    -not -path '*/build/*' \
    -not -path '*/coverage/*' \
    -not -path '*/venv/*'
}

# Kickstart the oh-my-zsh framework.
plugins=(git z docker vagrant tmux)
ZSH_THEME='llama'

source $ZSH/oh-my-zsh.sh
export PATH="$(yarn global bin)":"$PATH"
export PATH="$N_PREFIX"/bin:"$PATH"

# Not all functions and aliases should be shared.
if [[ -e ~/.custom-scripts/.zshrc ]]; then
  source ~/.custom-scripts/.zshrc
fi
