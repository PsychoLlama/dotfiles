prompt off # Disable default prompt settings (i.e. RPS1)

eval "$(starship init zsh)"
eval "$(zoxide init zsh)"


##################
# Shell Settings #
##################

# Don't stifle error codes in pipelines.
setopt pipefail

# Change directories without requiring `cd`.
setopt auto_cd

# Automatically put navigation history in the dir stack.
setopt auto_pushd pushd_ignore_dups

# Don't store duplicates, inline functions, or commands starting with a space
# character (incognito mode) in zsh history.
setopt hist_ignore_all_dups hist_no_functions hist_ignore_space

# Prettify entries and immediately store them in history.
setopt hist_reduce_blanks share_history

# Allow comments in interactive shells.
setopt interactive_comments


###########
# Aliases #
###########

# Easily navigate the dir stack.
for i in {1..5}; do
  alias "$i"="cd +$i"
done


##################
# Key Management #
##################

export SSH_AUTH_SOCK=/tmp/ssh-agent.sock

if [[ ! -S "$SSH_AUTH_SOCK" ]]; then
  eval "$(ssh-agent -a "$SSH_AUTH_SOCK" > /dev/null)"
fi


#########################
# Convenience Functions #
#########################

# Open the editor.
function v {
  if (( $# > 0 )); then
    nvim -p "$@"
  else
    nvim .
  fi
}

# Fuzzy find and edit a file.
function vf {
  local result="$(fd --type file | sk)"

  if [[ -z "$result" ]]; then
    return
  fi

  nvim "$result"
}

# Jump down to a directory using fuzzy search.
function cf {
  local result="$(fd --type directory | sk)"

  if [[ -z "$result" ]]; then
    return
  fi

  cd "$result"
}

# Show git status.
function s {
  if git rev-parse --is-inside-work-tree &> /dev/null; then
    git status
  else
    echo 'Not a git repo.' >&2
  fi
}

# `mkdir` and `cd` in one move.
function mkcd {
  mkdir --parents "$1"
  cd "$1"
}

# Open modified files in the editor.
function vh {
  local files=($(git ls-files --modified --others --exclude-standard 2> /dev/null))

  if [[ "$?" != 0 ]] || (( ${#files} == 0 )); then
    echo 'No modified files.' > /dev/stderr
    return 1
  fi

  nvim -p "${files[@]}"
}

# Open search results in the editor.
function vs {
  local files=($(rg "$@" -l))

  if (( ${#files} == 0 )); then
    echo 'No results.' > /dev/stderr
    return 1
  fi

  nvim -p "${files[@]}"
}

# Encrypt stdin using public keys from GitHub.
function encrypt {
  if [[ -z "$1" ]]; then
    echo 'Expected a username.' > /dev/stderr
    return 1
  fi

  local args=('-a')
  curl -sSL "https://github.com/$1.keys" | while read -r identity; do
    local file="$(mktemp)"
    echo "$identity" > "$file"
    args=("${args[@]}" '-R' "$file")
  done

  rage "${args[@]}" -
}

# Decrypt stdin using the SSH private key.
function decrypt {
  rage -d -i ~/.ssh/id_ed25519 -
}
