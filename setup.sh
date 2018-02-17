#!/usr/bin/env bash
set -e

# Supported environments:
# - Ubuntu Xenial (docker compatible)
# - OS X High Sierra

function cmd_exists {
  command -v "$1" &> /dev/null
}

function as_root() {
  # This could be run in a docker container.
  if [[ "$(whoami)" == "root" ]]; then
    "$@"
  elif cmd_exists sudo; then
    command sudo "$@"
  else
    echo "Root privileges required for command '$*'."
    echo "Install sudo then try again."
    exit 1
  fi
}

if cmd_exists apt-get; then
  as_root apt-get update

  # Install gcc (implicit rustc dependency - may change in the future)
  if ! cmd_exists add-apt-repository && ! cmd_exists gcc; then
    as_root apt-get install -y software-properties-common
    as_root add-apt-repository -y ppa:ubuntu-toolchain-r/test
    as_root apt-get update

    as_root apt-get install -y gcc-6
  fi
fi

# Get `curl` command. Required for rustup install script.
if ! cmd_exists curl; then
  if cmd_exists apt-get; then
    as_root apt-get install -y curl
  else
    echo 'Manually install curl, then try again.'
    exit 1
  fi
fi

# Resolve the absolute path of the current file.
pushd "$(dirname "$0")" &> /dev/null
DOTFILES_DIR="$(pwd -P)"
popd &> /dev/null

# Install Rust
if ! cmd_exists rustup; then
  RUST_INSTALLER_DEST=/tmp/rustup-installer.sh
  curl https://sh.rustup.rs -sSf > "$RUST_INSTALLER_DEST"

  chmod +x "$RUST_INSTALLER_DEST"
  "$RUST_INSTALLER_DEST" -y --default-toolchain nightly

  PATH="$PATH":~/.cargo/bin
fi

# Compile dotfiles
pushd "$DOTFILES_DIR" > /dev/null
cargo build --color always
popd > /dev/null

# Make the `dotfiles` command.
as_root ln -sf "$DOTFILES_DIR/dotfiles.sh" "/usr/local/bin/dotfiles"

# Some installs (like neovim) depend on these symlinks.
dotfiles link > /dev/null

# oh-my-zsh freaks if it sees another .zshrc file.
rm ~/.zshrc
dotfiles install
dotfiles link > /dev/null

# Log out for the new shell to take effect.
if [[ "$SHELL" != "$(which zsh)" ]]; then
  echo "All done. Log out for the changes to take effect."
fi