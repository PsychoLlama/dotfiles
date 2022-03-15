# Use Vim keybindings
set -o vi

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

# Show information about a nix package.
function gist {
  if (( $# < 1 )); then
    echo 'Requires a package name.' >&2
    return 1
  fi

  nix eval --offline --json --impure --expr "
  let
    flake = builtins.getFlake \"nixpkgs\";
    pkgs = flake.legacyPackages.\${builtins.currentSystem};

  in pkgs.$1.meta
  " | jq -r '.description, .homepage'
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

# Manage git (p)rojects.
function p {
  local user repo project_dir=~/projects

  if (( $# < 1 )); then
    echo 'What do you want to clone?' >&2
    return 1
  fi

  # Read out $user/$repo pattern.
  IFS=/ read -r user repo <<< "$1"

  # Allow a shorthand "$repo" assuming our own username.
  if [[ -z "$repo" ]]; then
    repo="$user"
    user="PsychoLlama"
  fi

  # Force usernames to be lowercase, otherwise you could make quite a mess.
  # Project names retain case for DX.
  user="$(echo "$user" | tr '[:upper:]' '[:lower:]')"
  repo="${repo/.git/}"

  local repo_path="$project_dir/$user/$repo"

  if [[ ! -d "$repo_path" ]]; then
    git clone "git@github.com:$user/$repo" "$repo_path" "${@:2}" || return $?
  fi

  cd "$repo_path"
}
