#!/usr/bin/env bash
set -e

DOTFILES_BIN_DIR=/usr/local/bin

# Resolve the absolute path of the current file.
pushd `dirname $0` &> /dev/null
DOTFILES_DIR=`pwd -P`
popd &> /dev/null

# Make sure the cache is fresh.
if which apt-get &> /dev/null; then
  sudo apt-get update
fi

# Symlink the dotfiles command.
sudo ln -sf "$DOTFILES_DIR/dotfiles.sh" "$DOTFILES_BIN_DIR/dotfiles"

# Make sure `dotfiles` exists as a command.
if ! which dotfiles &> /dev/null; then
  export PATH="$DOTFILES_BIN_DIR:$PATH"
fi

dotfiles link &> /dev/null

# oh-my-zsh gets all huffy when it sees another .zshrc file.
unlink ~/.zshrc
dotfiles install

dotfiles link &> /dev/null

# Log out for the new shell to take effect.
if [[ "$SHELL" != `which zsh` ]]; then
  echo "All done. Log out for the changes to take effect."
fi
