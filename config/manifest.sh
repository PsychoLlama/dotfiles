#!/usr/bin/env bash
# shellcheck disable=SC1090
set -e

function get_dotfiles_manifest {
  echo ~/.tmuxinator/edit.yml tmux-workspaces/edit.yml
  echo ~/.config/alacritty.yml alacritty.yml
  echo ~/.hushlogin empty-file.txt
  echo ~/.gitconfig git.ini
  echo ~/.tmux.conf tmux.conf
  echo ~/.ripgrep ripgrep.rc
  echo ~/.zshrc init.zsh

  local symlink_manifest=~/dotfiles-env/config/manifest.sh
  if [[ -f "$symlink_manifest" ]]; then
    source "$symlink_manifest"
  fi
}
