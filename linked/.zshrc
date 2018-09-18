#!/usr/bin/env bash

### Variables ###
export VIRTUAL_ENV_DISABLE_PROMPT=1
export TERM=screen-256color
export ZSH=~/.oh-my-zsh
export EDITOR=nvim
export N_PREFIX=~/.n
export PATH="$N_PREFIX/bin:$PATH"
export PATH=~/.cargo/bin:"$PATH"
export MANPAGER='nvim -c "setfiletype man" -'
export fpath=("$(dotfiles dir)/completions" $fpath)


### Aliases ###
alias g='git'
alias b='git branch --verbose'
alias :qa='tmux kill-session'
alias empty='empty-trash'
alias ch='git checkout'
alias D='cd ~/Desktop'
alias c='git commit'
alias sudo='sudo '
alias H='git diff HEAD'
alias HH='git diff HEAD~1'
alias HHH='git diff HEAD~2'

# Branch "dirstack"
alias b1='git checkout "$(git branch-history 1 | tail -1)"'
alias b2='git checkout "$(git branch-history 2 | tail -1)"'
alias b3='git checkout "$(git branch-history 3 | tail -1)"'
alias b4='git checkout "$(git branch-history 4 | tail -1)"'
alias b5='git checkout "$(git branch-history 5 | tail -1)"'
alias b6='git checkout "$(git branch-history 6 | tail -1)"'
alias b7='git checkout "$(git branch-history 7 | tail -1)"'
alias b8='git checkout "$(git branch-history 8 | tail -1)"'
alias b9='git checkout "$(git branch-history 9 | tail -1)"'

### Functions ###
function mkcd {
  mkdir -p "$1"
  cd "$1" || return 1
}

function edit {
  local session_name="$1"

  if [[ -z "$session_name" ]]; then
    session_name="$(basename "$PWD")"
  fi

  tmuxinator start edit -n "$session_name"
}

function s {
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo "Not a git repo."
    return 1
  fi

  git status
}

function t {
  # Arguments? Treat `t` like an alias.
  if [[ $# != 0 ]]; then
    tmux "$@"
    return $?
  fi

  if [[ -n "$TMUX" ]]; then
    echo "Chill out dude. One tmux is enough." 1>&2
    return 1
  fi

  # Attach to the most recent session, or create one.
  local recent_session="$(tmux ls 2> /dev/null | awk 'END {print $1}' | sed 's/://')"

  if [[ -z "$recent_session" ]]; then
    tmux new
    return $?
  fi

  tmux attach -t "$recent_session"
  return $?
}

function tw {
  local utils="$(dotfiles dir)"/utils
  read script runner <<< "$("$utils"/get-package-test-script.js)"

  if [[ -z "$script" ]]; then
    return 1
  fi

  case "$runner" in
    "jest" | "react-scripts")
      yarn --silent run "$script" --watch --collectCoverage=false --testPathPattern "$1"
      ;;
    "mocha")
      yarn --silent run "$script" --watch --reporter min "$1"
      ;;
    *)
      yarn --silent run "$script" --watch
      ;;
  esac
}

function tws {
  yarn --silent run "$1" --watch --collectCoverage=false --testPathPattern "$2"
}

function gag {
  ag "$@" \
    --ignore '*bundle.*' \
    --ignore '*.js.map' \
    --ignore yarn.lock \
    --ignore schema.js \
    --ignore node_modules/ \
    --ignore dist/ \
    --ignore coverage/ \
    --ignore flow-typed/ \
    --ignore static/ \
    --ignore build/
}

function gg {
  gag "$@" --ignore __tests__ --ignore tests
}

function v {
  if [[ "$#" == "0" ]]; then
    nvim .
  else
    nvim -p "$@"
  fi
}

function vs {
  if [[ "$#" == "0" ]]; then
    echo "WRONG! Try again."
    return 1
  fi

  nvim -p $(gag "$@" -l)
}

function vh {
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo 'Not a repo, loser.'
    return 1
  fi

  local repo_root="$(git rev-parse --show-toplevel)"
  local filepaths=()

  git diff --name-only | while read filepath; do
    filepaths=($filepaths "$repo_root/$filepath")
  done

  if [[ "${#filepaths[@]}" == 0 ]]; then
    echo 'No changes :/'
    return 1
  fi

  v "${filepaths[@]}"
}

function vf {
  local file
  if file="$(fzf)"; then
    v "$file"
  fi
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

# Export Lua module path variables.
if command -v luarocks > /dev/null; then
  eval "$(luarocks path)"
fi

# Kickstart the oh-my-zsh framework.
plugins=(docker)
source "$ZSH/oh-my-zsh.sh"

ARTIFACTS="$(dotfiles dir)/artifacts"

# Adds `z` function (https://github.com/rupa/z).
source "$ARTIFACTS/z/z.sh"

# Use my custom shell prompt.
source "$ARTIFACTS/llama-theme.sh"

# Check for computer-specific shell utils.
if [[ -e ~/dotfiles-env/.zshrc ]]; then
  source ~/dotfiles-env/.zshrc
fi

### Completions ###
compdef t=tmux

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
