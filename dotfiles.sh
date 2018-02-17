#!/usr/bin/env bash
set -e

DOTFILES_DIR=$(dirname "$(readlink "$0")")

# Update the dotfiles repo.
function update {
  pushd "$DOTFILES_DIR" &> /dev/null

  echo Updating dotfiles...
  git pull origin
  cargo build --color always

  dotfiles install
  dotfiles link

  popd &> /dev/null
}

# Show the usage screen.
function print_help {

  # Add an indent level to messages.
  function indent {
    local indention=""

    for _ in $(seq 1 "$1"); do
      indention="$indention    "
    done

    echo "$indention$2"
  }

  echo ""

  indent 1 "Usage: dotfiles <command>"
  indent 2 "link    - Symlink everything in 'dotfiles/linked'"
  indent 2 "update  - Pull dotfile changes from git"
  indent 2 "install - Install system-wide dependencies"
  indent 2 "dir     - Print the dotfiles directory"

  echo ""
}

# Install all the dependencies.
function install {
  bash "$DOTFILES_DIR/install.sh"
}

# Figure out what command to run.
case "$1" in
  "link")
    # Call through to Rust. Eventually, this dotfiles
    # script will be replaced with a Rust implementation.
    "$DOTFILES_DIR"/target/debug/dotfiles link
    ;;
  "update")
    update
    ;;
  "install")
    install
    ;;
  "dir")
    echo "$DOTFILES_DIR"
    ;;
  *)
    if [[ ! -z "$1" && "$1" != "--help" ]]; then
      echo "Invalid command '$*'. Showing help instead."
      print_help

      # Probably a typo. Make them feel the pain!
      exit 1
    fi

    # Exit safely
    print_help
    ;;
esac
