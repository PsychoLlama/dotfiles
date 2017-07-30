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

  echo ""
}

# Install all the dependencies.
function dotfiles_install {

  # Whether a command exists.
  function has {
    $(which "$1" &> /dev/null)

    return $?
  }

  local INSTALL_CMD=""

  if has brew; then
    INSTALL_CMD="brew install"
  elif has apt-get; then
    INSTALL_CMD="sudo apt-get install -y"
  elif has yum; then
    INSTALL_CMD="sudo yum install"
  elif has zypper; then
    INSTALL_CMD="sudo zypper install"
  else
    echo "How do you install things on this machine?!?"
  fi

  # Best-effort install. Prone to breakage.
  function install {
    $INSTALL_CMD "$1" 1> /dev/null
  }

  # Install to a /tmp file and verify the integrity hash.
  function install_via_curl {
    local file_name=/tmp/`basename $1`
    curl -fsSL "$1" > "$file_name"

    local integrity=`openssl sha1 "$file_name" | awk '{print $2}'`

    if [[ "$integrity" != "$2" ]]; then
      echo "Hmmm, an install script looks sketchy. The integrity doesn't match."
      echo "    URL: $1"
      exit 1
    fi

    bash "$file_name"
    rm "$file_name"
  }

  # Install the thing if it doesn't exist.
  function ensure {
    if has "$1"; then
      return 0
    fi

    install "$1"
  }

  ensure curl
  ensure openssl

  # Install zsh
  if ! has zsh; then
    install zsh
    chsh -s zsh
    zsh
  fi

  # Install oh-my-zsh
  if [[ -z "$ZSH" ]]; then
    install_via_curl https://cdn.rawgit.com/robbyrussell/oh-my-zsh/d848c94804918138375041a9f800f401bec12068/tools/install.sh f423ddfb1d0b6a849b229be5b07a032c10e13c6f
  fi
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
