#!/usr/bin/env bash
set -e

DOTFILES_BIN_DIR=/usr/local/bin

# Resolve the absolute path of the current file.
pushd `dirname $0` &> /dev/null
DOTFILES_DIR=`pwd -P`
popd &> /dev/null

# Symlink the dotfiles command.
touch ~/.hushlogin
ln -sf "$DOTFILES_DIR/dotfiles.sh" "$DOTFILES_BIN_DIR/dotfiles"
bash "$DOTFILES_DIR/install.sh"

if which dotfiles &> /dev/null; then
  echo 'It worked! Now you should have a "dotfiles" command.'
else
  echo "Hmmm, that didn't work. Is ${DOTFILES_BIN_DIR} in your path?"
  exit 1
fi
