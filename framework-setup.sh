#!/usr/bin/env bash
set -e

DOTFILES_BIN_DIR=/usr/local/bin

# Resolve the absolute path of the current file.
pushd `dirname $0` &> /dev/null
DOTFILES_DIR=`pwd -P`
popd &> /dev/null

function update_apt_cache {
  local cache=/var/cache/apt/pkgcache.bin
  local elapsed=$(expr `date +%s` - `stat --format=%Y $cache`)

  # Update if the cache is more than a day old.
  if [[ "$elapsed" > 86400 ]]; then
    sudo apt-get update
  fi
}

if which apt-get &> /dev/null; then
  update_apt_cache
fi

# Symlink the dotfiles command.
sudo ln -sf "$DOTFILES_DIR/dotfiles.sh" "$DOTFILES_BIN_DIR/dotfiles"
bash "$DOTFILES_DIR/install.sh"

if which dotfiles &> /dev/null; then
  echo 'It worked! Now you should have a "dotfiles" command.'
else
  echo "Hmmm, that didn't work. Is ${DOTFILES_BIN_DIR} in your path?"
  exit 1
fi
