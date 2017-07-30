#!/usr/bin/env bash -e
DOTFILES_DIR=$(dirname `readlink $0`)

# Add symlinks to each file.
function dotfiles_link_everything {
  function resolve_file_location {
    echo "$DOTFILES_DIR/linked/$1"
  }

  # Before linking a file, make sure it exists.
  function verify_file_exists {
    local file=`resolve_file_location $1`

    if [[ ! -e "$file" ]]; then
      echo "Failure: $1"
      echo ""
      echo "There's no mention of '$1' in the dotfile links."
      echo "    (looked here: $file)"

      exit 1
    fi
  }

  # Symlink a file from `linked/`.
  function link {
    local file_to_link=`resolve_file_location $1`

    # Validate the link target.
    verify_file_exists "$1" "$2"

    # Make sure the directory exists.
    mkdir -p `dirname $2`

    # Create symlink.
    ln -sf "$DOTFILES_DIR/linked/$1" "$2"

    echo "Success: $1"
  }

  link .zshrc ~/.zshrc
  link .tmux.conf ~/.tmux.conf
  link init.vim ~/.config/nvim/init.vim
  link .gitconfig ~/.gitconfig
  link .tmuxinator ~/.tmuxinator
}

# Update the dotfiles repo.
function dotfiles_update {
  pushd "$DOTFILES_DIR" &> /dev/null

  echo Updating dotfiles...
  git pull origin master

  popd &> /dev/null
}

# Show the usage screen.
function dotfiles_print_help {

  # Add an indent level to messages.
  function indent {
    local indention=""

    for _ in `seq 1 "$1"`; do
      indention=`echo "$indention    "`
    done

    echo "$indention$2"
  }

  echo ""

  indent 1 "Usage: dotfiles [command]"
  indent 2 "link    - Symlink everything in 'dotfiles/linked'"
  indent 2 "update  - Pull dotfile changes from git"
  indent 2 "install - Install system-wide dependencies"
  indent 2 "dir     - Print the dotfiles directory"
  indent 2 "repo    - Synonym for 'dotfiles dir'"

  echo ""
}

# Install all the dependencies.
function dotfiles_install {
  bash "$DOTFILES_DIR/install.sh"
}

# Figure out what command to run.
case "$1" in
  "link")
    dotfiles_link_everything
    ;;
  "update")
    dotfiles_update
    ;;
  "install")
    dotfiles_install
    ;;
  "dir" | "repo")
    echo "$DOTFILES_DIR"
    ;;
  *)
    if [[ ! -z "$1" && "$1" != "--help" ]]; then
      echo "Invalid command '$@'. Showing help instead."
      dotfiles_print_help

      # Probably a typo. Make them feel the pain!
      exit 1
    else

      # Exit safely
      dotfiles_print_help
    fi
    ;;
esac
