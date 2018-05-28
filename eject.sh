#!/usr/bin/env zsh
set -e

parachute=parachute.tar.gz

# Bash 4 is required for hash tables, but mac is stuck on bash 3.
# Zsh to the rescue.
typeset -A files
files[.viminfo]=~/.viminfo
files[vim-backup]=~/.vim/backup/
files[vim-undodir]=~/.vim/undodir/
files[vim-metrics.json]=~/.vim/metrics.json
files[.node_repl_history]=~/.node_repl_history
files[.python_history]=~/.python_history
files[.zsh_history]=~/.zsh_history
files[.z]=~/.z

function warn {
  echo -e "WARN: $*" >&2
}

manifest=~/dotfiles-env/eject-manifest.sh
if [[ -f "$manifest" ]]; then
  source "$manifest"

  # Merge the env & base manifests.
  for extension in "${(k)DOTFILES_MANIFEST}"; do
    files[$extension]="${DOTFILES_MANIFEST[$extension]}"
  done
fi

# Timestamp as <date>--<time>
# Example: 2018-05-18--16-05
date="$(date +'%Y-%m-%d--%H-%M')"
target="$(dotfiles dir)/$date"
mkdir -p "$target"

for file_dest in "${(k)files[@]}"; do
  file_source="${files[$file_dest]}"

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
