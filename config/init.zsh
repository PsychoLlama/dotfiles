### Variables ###
export VIRTUAL_ENV_DISABLE_PROMPT=1
export SKIM_DEFAULT_COMMAND='fd'
export COLORTERM=truecolor
export ZSH=~/.nix-profile/share/oh-my-zsh
export EDITOR=nvim
export PATH=~/.cargo/bin:"$PATH"
export MANPAGER='nvim -c "setfiletype man" -'
export RIPGREP_CONFIG_PATH=~/.ripgrep
export BAT_STYLE=changes
export BAT_THEME=TwoDark
export NIX_IGNORE_SYMLINK_STORE=1 # Catalina ruins everything.


### Aliases ###
alias g='git'
alias b='git branch'
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

# Fancy alternatives
alias cat=bat

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

# Poke around another project without the commitment.
function inspect {
  local remote="$1"
  local target="$(mktemp -d)"

  if [[ "${1:0:4}" != http ]] && [[ "${1:0:3}" != git ]]; then
    remote="https://github.com/$remote"
  fi

  git clone --quiet --depth=1 "$remote" "$target"
  nvim "$target"
  rm -rf "$target"
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
  local cache_file=/tmp/vf-last-result
  local file

  case "$1" in
    '--last' | '-l')
      file="$(command cat "$cache_file")"

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

source ~/.nix-profile/etc/profile.d/nix.sh

plugins=(docker)
source ~/.nix-profile/share/oh-my-zsh/oh-my-zsh.sh
source ~/.nix-profile/share/zsh-rustup-completion/rustup.plugin.zsh
source ~/.nix-profile/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.nix-profile/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.nix-profile/share/llama.zsh-theme/llama.zsh-theme

# Use fnm-managed node installation.
if [[ -d ~/.fnm ]]; then
  export PATH="$HOME/.fnm:$PATH"
  eval "$(fnm env)"
fi

# Initialize zoxide (`z` command)
eval "$(zoxide init zsh)"

# Check for computer-specific shell utils.
if [[ -e ~/dotfiles-env/.zshrc ]]; then
  source ~/dotfiles-env/.zshrc
fi

### Completions ###
compdef t=tmux
