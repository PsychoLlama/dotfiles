#!/usr/bin/env bash
vimrc="
filetype off
set rtp+=~/.vim/plugged/vader.vim
set rtp+=$(dotfiles dir)/editor
filetype plugin indent on
syntax enable
"

nvim -Nu <(echo "$vimrc") -c 'Vader! **/*.vader'
