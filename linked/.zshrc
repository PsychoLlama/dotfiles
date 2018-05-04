#!/usr/bin/env bash

### Variables ###
export VIRTUAL_ENV_DISABLE_PROMPT=1
export TERM=screen-256color
export ZSH=~/.oh-my-zsh
export EDITOR=nvim

export N_PREFIX=~/.n
export PATH="$N_PREFIX/bin:$PATH"
export PATH=~/.cargo/bin:"$PATH"


### Aliases ###
alias g='git'
alias b='git branch --verbose'
alias :qa='tmux kill-session'
alias empty='empty-trash'
alias ch='git checkout'
alias D='cd ~/Desktop'
alias c='git commit'
alias sudo='sudo '
alias :q='exit'
alias H='git diff HEAD'
alias HH='git diff HEAD~1'
alias HHH='git diff HEAD~2'


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

# Open man pages in Neovim.
function man {
  if [[ "$#" == 0 ]]; then
    echo 'I find your lack of arguments disturbing.'
    return 1
  fi

  if [[ "$#" != 1 ]]; then
    command man "$@"
    return "$?"
  fi

  # Prevent `local` from obscuring the exit code.
  local page_contents
  local exit_code

  page_contents="$(command man "$@" 2>&1)"
  exit_code="$?"

  if [[ "$exit_code" != 0 ]]; then
    echo "$page_contents"
    return "$exit_code"
  fi

  nvim -c 'setfiletype man' - <<< "$page_contents"
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

function vp {
  local file_name="${1/.js/}"
  local test_file="__tests__/$file_name"

  if [[ -f "$test_file.spec.js" ]]; then
    test_file="$test_file.spec.js"
  elif [[ -f "$test_file.test.js" ]]; then
    test_file="__tests__/$file_name.test.js"
  elif [[ -d "__tests__" ]]; then
    test_file="__tests__/"
  else
    test_file="./"
  fi

  v "$file_name.js" "$test_file"
}

function _find_file_in_repo {
  local result="$(git ls-files | grep -i $1)"

  if [[ -n "$2" ]]; then
    result="$(awk "!$2 { print }" <<< "$result")"
  fi

  # Get the top-most result.
  result="$(head -1 <<< "$result")"

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
  local file="$(_find_file_in_repo "$1" "/test\\.js/")"

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
