#!/usr/bin/env bash
set -e

ARTIFACTS_DIR="$(dotfiles dir)/artifacts"

if [[ ! -e "$ARTIFACTS_DIR" ]]; then
  mkdir "$ARTIFACTS_DIR"
fi

function announce {
  if [[ -z "$VERBOSE" ]]; then
    return
  fi

  local msg="$*"
  local border=""

  for _ in $(seq ${#msg}); do
    border="$border#"
  done

  echo
  echo "$border"
  echo "$@"
  echo "$border"
  echo
}

# Whether a command exists.
function installed {
  which "$1" &> /dev/null

  return $?
}

INSTALL_CMD=""

if installed brew; then
  INSTALL_CMD="brew install"
elif installed apt-get; then
  INSTALL_CMD="sudo apt-get install -y"
elif installed yum; then
  INSTALL_CMD="sudo yum install"
elif installed pacman; then
  INSTALL_CMD="sudo pacman -S"
else
  echo "How do you install things on this machine?!?"
fi

# Best-effort install. Prone to breakage.
function install {
  $INSTALL_CMD $1 1> /dev/null
}

# Install the thing if it doesn't exist.
function ensure {
  if installed "$1"; then
    return
  fi

  announce Installing "$1"
  install "$1"
}

function verify_hash {
  local integrity="$(openssl sha1 <<< "$2")"

  # Some systems print junk before the hash.
  if ! installed brew; then
    integrity="$(echo "$integrity" | awk '{print $2}')"
  fi

  if [[ "$integrity" != "$1" ]]; then
    return 1
  fi

  return
}

# Install to a /tmp file and verify the integrity hash.
function install_via_curl {
  local install_script="$(curl -fsSL "$1")"

  if ! verify_hash "$2" "$install_script"; then
    echo "Hmmm, an install script looks sketchy. The integrity doesn't match."
    echo "    URL: $1"
    exit 1
  fi

  set +e
  bash <<< "$install_script"
  set -e
}

function ensure_apt_add_command {
  if installed add-apt-repository; then
    return
  fi

  install software-properties-common
  install python-software-properties
}

function install_make {
  if installed make; then
    return
  fi

  announce Installing make
  install build-essential
}

function install_zsh {
  if installed zsh; then
    return
  fi

  announce Installing zsh
  install zsh

  # Don't attempt to change shell on Travis CI.
  # The build will hang forever.
  if [[ -n "$CI" ]]; then
    return
  fi

  chsh -s "$(which zsh)"
}

function install_oh_my_zsh {
  if [[ -d ~/.oh-my-zsh || -n "$CI" ]]; then
    return
  fi

  announce Installing oh-my-zsh
  install_via_curl https://cdn.rawgit.com/robbyrussell/oh-my-zsh/d848c94804918138375041a9f800f401bec12068/tools/install.sh f423ddfb1d0b6a849b229be5b07a032c10e13c6f &> /dev/null
}

function install_yarn {
  if installed yarn; then
    return
  fi

  announce Installing yarn
  local pkg="yarn"

  # Don't install node too.
  if installed brew; then
    pkg="yarn --ignore-dependencies"
  fi

  install "$pkg"
}

function install_ruby {
  local pkg="ruby"
  if installed apt-get; then
    pkg="ruby2.4"
  fi

  if installed "$pkg"; then
    return
  fi

  announce Installing ruby

  install "$pkg"
}

function install_tmux {
  if installed tmux; then
    return
  fi

  announce Installing tmux

  local DEST="$ARTIFACTS_DIR/tmux"
  git clone https://github.com/tmux/tmux.git "$DEST"
  pushd "$DEST" > /dev/null

  git checkout --quiet bd71cbbe276432ce8869baa0f2d55433e1ab820c
  sh autogen.sh &> /dev/null

  # Assumes the only possible failure is missing packages.
  ./configure > /dev/null || {
    sudo apt-get install -y libevent-dev libncurses5-dev
    ./configure
  }

  make &> /dev/null
  sudo make install &> /dev/null

  rm -rf "$DEST"
  popd > /dev/null
}

function install_tmuxinator {
  if installed tmuxinator; then
    return
  fi

  announce Installing tmuxinator
  sudo gem install tmuxinator
}

function install_silver_searcher {
  if installed ag; then
    return
  fi

  announce Installing the silver searcher
  local pkg_name="the_silver_searcher"

  # Goes by a different name on aptitude.
  if installed apt-get; then
    pkg_name="silversearcher-ag"
  fi

  install "$pkg_name"
}

function install_llama_zsh_theme {
  local DEST="$ARTIFACTS_DIR/llama-theme.sh"

  if [[ -f "$DEST" || -n "$CI" ]]; then
    return
  fi

  announce Installing llama.zsh-theme
  local theme="$(curl https://cdn.rawgit.com/PsychoLlama/llama.zsh-theme/53c3ba8079378bc02b159aa7c6275ade1fddaa58/llama.zsh-theme)"

  if ! verify_hash "af891adfd8a9684e31ebadb7830a6a95bcceda9c" "$theme"; then
    echo "llama zsh theme install failed. The hash doesn't match."
    exit 1
  fi

  echo "$theme" > "$DEST"
}

# A node version manager, alternative to nvm.
function install_n {
  if installed n; then
    return
  fi

  announce Installing n
  git clone https://github.com/tj/n.git ~/.n-bin &> /dev/null

  pushd ~/.n-bin &> /dev/null
  sudo make &> /dev/null
  popd &> /dev/null

  rm -rf ~/.n-bin
}

function install_z {
  local DEST="$ARTIFACTS_DIR/z"

  if [[ -e "$DEST" ]]; then
    return
  fi

  git clone https://github.com/rupa/z.git "$DEST"
  touch ~/.z
}

function install_node {
  if installed node; then
    return
  fi

  announce Installing node
  N_PREFIX=~/.n n latest
}

function install_vim_plug {
  local target=~/.local/share/nvim/site/autoload/plug.vim

  if [[ -f "$target" ]]; then
    return
  fi

  announce Installing vim-plug
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

function install_neovim {
  if installed nvim; then
    return
  fi

  announce Installing neovim
  local pkg="neovim"
  if installed apt-get; then
    sudo apt-get update

    install python-dev
    install python-pip
    install python3-dev
    install python3-pip
  fi

  install "$pkg"
  mkdir -p ~/.vim/backup
  sudo chmod 777 ~/.vim/backup
}

function install_python_neovim_plugin {
  if python3 -c 'import neovim' &> /dev/null; then
    return
  fi

  announce Installing python neovim plugin
  sudo -H pip3 install neovim > /dev/null
}

function install_neovim_plugins {
  local hash_file="$ARTIFACTS_DIR/plugins-hash.txt"
  local plugins_sha="$(openssl sha256 editor/plugins.vim | awk '{print $2}')"
  local last_sha="$(cat "$hash_file" 2> /dev/null || echo "no prior hash")"

  # Only re-install plugins when the plugins.vim file changes.
  if [[ "$last_sha" == "$plugins_sha" ]]; then
    return
  fi

  echo "$plugins_sha" > "$hash_file"
  announce Installing neovim plugins

  # Source the vimrc in non-interactive mode.
  nvim\
    -u /dev/null\
    -c "silent so ~/.config/nvim/init.vim"\
    -c "PlugInstall"\
    -c "UpdateRemotePlugins"\
    -c "qa"

  # Don't ask on startup.
  mkdir -p ~/.vim/plugged/vim-notes/misc/notes/user/
}

function install_vint {
  if installed vint; then
    return
  fi

  # Pre-xenial distros ship with a horribly dated setuptools version.
  local setuptools_version="$(python3 -m easy_install --version | awk '{print $2}')"

  if [[ "${setuptools_version//.*/}" -lt "30" ]]; then
    sudo -H pip3 install -U setuptools
  fi

  announce Installing vint
  sudo -H pip3 install vim-vint > /dev/null
}

function install_pylint {
  if installed pylint; then
    return
  fi

  announce Installing pylint
  if installed brew; then
    pip3 install pylint
  else
    install pylint
  fi
}

function install_rustfmt {
  local list="$(rustup component list)"
  local pkg="$(grep rustfmt-preview <<< "$list")"

  if [[ "$pkg" =~ installed ]]; then
    return
  fi

  announce Installing rustfmt
  rustup component add rustfmt-preview
}

function install_shellcheck {
  if ! installed shellcheck; then
    announce Installing shellcheck
    install shellcheck &> /dev/null
  fi
}

ensure curl
ensure openssl
ensure automake
ensure python3

# PPAs.
if installed apt-get; then
  ensure_apt_add_command
  UPDATE=

  # Yarn.
  if ! installed yarn; then
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
    UPDATE=1
  fi

  if ! installed ruby2.4; then

    # Ruby PPA (ruby-full doesn't work pre-xenial).
    sudo add-apt-repository -y ppa:brightbox/ruby-ng
    UPDATE=1
  fi

  if ! installed nvim; then
    neovim_ppa="ppa:neovim-ppa/stable"
    source /etc/os-release

    if [[ "${VERSION//.*/}" -lt "16" ]]; then
      neovim_ppa="ppa:neovim-ppa/unstable"
    fi

    sudo add-apt-repository -y "$neovim_ppa"
    UPDATE=1
  fi

  if [[ -n "$UPDATE" ]]; then
    sudo apt-get update
  fi
fi

install_make
install_zsh
install_oh_my_zsh
install_yarn
install_ruby
install_tmux
install_tmuxinator
install_silver_searcher
install_llama_zsh_theme
install_n
install_z
install_node
install_vim_plug
install_neovim
install_python_neovim_plugin
install_neovim_plugins
install_vint
install_pylint
install_rustfmt
install_shellcheck
