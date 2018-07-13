#!/usr/bin/env bash
function get_dotfiles_manifest {
  echo ~/.tmuxinator/edit.yml .tmuxinator/edit.yml
  echo ~/.config/nvim/init.vim init.vim
  echo ~/.gitconfig .gitconfig
  echo ~/.tmux.conf .tmux.conf
  echo ~/.zshrc .zshrc
}
