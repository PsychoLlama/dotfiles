#!/usr/bin/env bash
# shellcheck disable=SC1090
set -e

parachute=parachute.tar.gz
files=(
  ~/.viminfo
  ~/.vim/backup/
  ~/.vim/undodir/
  ~/.vim/metrics.json
  ~/.node_repl_history
  ~/.python_history
  ~/.zsh_history
  ~/.z
)

function warn {
  echo -e "WARN: $*" >&2
}

manifest=~/dotfiles-env/eject-manifest.sh
if [[ -f "$manifest" && -e "$manifest" ]]; then
  source "$manifest"
  files=("${files[@]}" "${DOTFILES_MANIFEST[@]}")
fi

# Timestamp as <date>--<time>
# Example: 2018-05-18--16-05
date="$(date +'%Y-%m-%d--%H-%M')"
target="$(dotfiles dir)/$date"
mkdir -p "$target"

for file in "${files[@]}"; do
  # Skip files that don't exist.
  if [[ ! -e "$file" ]]; then
    warn "No file or directory. Skipping $file"
    continue
  fi

  # `cp -R` handles directories with trailing slashes unexpectedly.
  # TODO: make sure this syntax won't kill us all someday.
  file="${file%\/}"

  # Preserving file attributes & follow symlinks.
  cp -LRp "$file" "$target/"
done

pushd "$(dotfiles dir)" &> /dev/null
tar -czf "$parachute" "$date" &> /dev/null
rm -rf "$target"
popd &> /dev/null

echo "$parachute"
