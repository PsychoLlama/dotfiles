#!/usr/bin/env bash
# shellcheck disable=SC1090
set -e

function get_dotfiles_manifest {
  echo ~/.tmuxinator/edit.yml .tmuxinator/edit.yml
  echo ~/.config/nvim/init.vim init.vim
  echo ~/.gitconfig .gitconfig
  echo ~/.tmux.conf .tmux.conf
  echo ~/.zshrc .zshrc

  local symlink_manifest=~/dotfiles-env/linked/manifest.sh
  if [[ -f "$symlink_manifest" ]]; then
    source "$symlink_manifest"
  fi
}
