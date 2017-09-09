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
alias v='nvim -p'
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

function vs {
  nvim -p $(gag "$@" -l)
}

function goto {
  local result="$(git ls-files | grep $1 | head -1)"

  if [[ -z "$result" ]]; then
    echo "Nothing matched."
    return
  fi

  cd "$(dirname "$result")" || return 1
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
