#!/usr/bin/env bash

### Variables ###
export VIRTUAL_ENV_DISABLE_PROMPT=1
export SKIM_DEFAULT_COMMAND='fd'
export TERM=screen-256color
export ZSH=~/.oh-my-zsh
export EDITOR=nvim
export N_PREFIX=~/.n
export PATH="$N_PREFIX/bin:$PATH"
export PATH=~/.cargo/bin:"$PATH"
export MANPAGER='nvim -c "setfiletype man" -'
export fpath=("$(dotfiles dir)/completions" $fpath)
export RIPGREP_CONFIG_PATH=~/.ripgrep


### Aliases ###
alias g='git'
alias b='git --no-pager branch --verbose'
alias :qa='tmux kill-session 2> /dev/null || exit'
alias empty='empty-trash'
alias ch='git checkout'
alias D='cd ~/Desktop'
alias c='git commit'
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

# Open a development environment in the current directory
function edit {
  local session_name="$1"

  if [[ -z "$session_name" ]]; then
    session_name="$(basename "$PWD")"
  fi

  tmuxinator start edit -n "$session_name"
}

# Faster `git status`
function s {
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo "Not a git repo."
    return 1
  fi

  git status
}

# tmux shorthand
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

# tmux attach with skim
function ta {
  local session="$(tmux list-sessions -F '#{session_name}' | sk)"
  if [[ -z "$session" ]]; then
    return 1
  fi

  if [[ -z "$TMUX" ]]; then
    tmux attach -t "$session"
  else
    tmux switch-client -t "$session"
  fi
}

# "test watch" with the given regex
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

# "test watch script" expects the name of the package.json script.
function tws {
  yarn --silent run "$1" --watch --collectCoverage=false --testPathPattern "$2"
}

function gg {
  rg "$@" --ignore-file <(echo "__tests__\ntests\nflow-typed")
}

# Like `nvim` but better
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

  nvim -p $(gg "$@" -l)
}

function vh {
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo 'Not a repo, loser.'
    return 1
  fi

  local repo_root="$(git rev-parse --show-toplevel)"
  local filepaths=()

  git diff HEAD --name-only | while read -r filepath; do
    filepaths=($filepaths "$repo_root/$filepath")
  done

  if [[ "${#filepaths[@]}" == 0 ]]; then
    echo 'No changes :/'
    return 1
  fi

  v "${filepaths[@]}"
}

function vf {
  local cache_file="$(dotfiles dir)/command-cache/vf-last-result"
  local file

  case "$1" in
    '--last' | '-l')
      file="$(cat "$cache_file")"

      if [[ -z "$file" ]]; then
        echo "There's no find history :/" >&2
        return 1
      fi

      nvim "$file"
      return $?
      ;;
    *)
      if file="$(fd | sk)"; then
        echo "${PWD}/$file" > "$cache_file"
        echo "$file"
        nvim "$file"
      fi
      ;;
  esac
}

function cf {
  local dir="$(fd --type d | sk)"

  if [[ -z "$dir" ]]; then
    return 1
  fi

  cd "$dir"
}

source "$(dotfiles dir)/artifacts/antigen/antigen.zsh"
antigen use oh-my-zsh
antigen bundle docker
antigen bundle rupa/z
antigen bundle pkulev/zsh-rustup-completion
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme PsychoLlama/llama.zsh-theme

# Check for computer-specific shell utils.
if [[ -e ~/dotfiles-env/.zshrc ]]; then
  source ~/dotfiles-env/.zshrc
fi

antigen apply

### Completions ###
compdef t=tmux
