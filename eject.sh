#!/usr/bin/env zsh
set -e

parachute=parachute.tar.gz

# Bash 4 is required for hash tables, but mac is stuck on bash 3.
# Zsh to the rescue.
typeset -A manifest
manifest[vim-info]=~/.viminfo
manifest[vim-backup]=~/.vim/backup/
manifest[vim-undo-history]=~/.vim/undodir/
manifest[vim-metrics]=~/.vim/metrics.json
manifest[repl-history-node]=~/.node_repl_history
manifest[repl-history-python]=~/.python_history
manifest[zsh-history]=~/.zsh_history
manifest[z-weights]=~/.z

function warn {
  echo -e "WARN: $*" >&2
}

env_manifest=~/dotfiles-env/eject-manifest.sh
if [[ -f "$env_manifest" ]]; then
  source "$env_manifest"

  # Merge the env & base manifests.
  for env_dest in "${(k)DOTFILES_MANIFEST[@]}"; do
    manifest[$env_dest]="${DOTFILES_MANIFEST[$env_dest]}"
  done
fi

# Timestamp as <date>--<time>
# Example: 2018-05-18--16-05
date="$(date +'%Y-%m-%d--%H-%M')"
target="$(dotfiles dir)/$date"
mkdir -p "$target"

for file_dest in "${(k)manifest[@]}"; do
  file_source="${manifest[$file_dest]}"

  # Skip files that don't exist.
  if [[ ! -e "$file_source" ]]; then
    warn "No file or directory. Skipping '$file_source'"
    continue
  fi

  # `cp -R` only copies children if directory names end in "/".
  if [[ -d "$file_source" && "${file_source[-1]}" == "/" ]]; then
    file_source="${file_source:0:-1}"
  fi

  # Preserving file attributes & follow symlinks.
  cp -LRp "$file_source" "$target/$file_dest"
done

pushd "$(dotfiles dir)" &> /dev/null
tar -czf "$parachute" "$date" &> /dev/null
rm -rf "$target"
popd &> /dev/null

if [[ "$PWD" != "$(dotfiles dir)" ]]; then
  mv "$(dotfiles dir)/$parachute" ./
fi

echo "$parachute"
