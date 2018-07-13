#!/usr/bin/env bash
set -e

git clone https://github.com/PsychoLlama/dotfiles.git ~/dotfiles
cd ~/dotfiles

VERBOSE=1 ./setup
