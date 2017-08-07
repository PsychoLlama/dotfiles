#!/usr/bin/env bash
set -e

function main {

  # Whether a command exists.
  function installed {
    $(which "$1" &> /dev/null)

    return $?
  }

  local INSTALL_CMD=""

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
      return 0
    fi

    install "$1"
  }

  function verify_hash {
    local integrity=`openssl sha1 <<< "$2"`

    # Some systems print junk before the hash.
    if ! installed brew; then
      integrity=`echo $integrity | awk '{print $2}'`
    fi

    if [[ "$integrity" != "$1" ]]; then
      return 1
    fi

    return 0
  }

  # Install to a /tmp file and verify the integrity hash.
  function install_via_curl {
    local install_script=`curl -fsSL "$1"`

    if ! verify_hash "$2" "$install_script"; then
      echo "Hmmm, an install script looks sketchy. The integrity doesn't match."
      echo "    URL: $1"
      exit 1
    fi

    eval "$install_script"
  }

  function ensure_apt_add_command {
    if installed add-apt-repository; then
      return 0
    fi

    install software-properties-common
    install python-software-properties
  }

  function install_make {
    if installed make; then
      return 0
    fi

    install build-essential
  }

  function install_zsh {
    if installed zsh; then
      return 0
    fi

    install zsh
    chsh -s `which zsh`
  }

  function install_oh_my_zsh {
    if [[ -d ~/.oh-my-zsh ]]; then
      return 0
    fi

    install_via_curl https://cdn.rawgit.com/robbyrussell/oh-my-zsh/d848c94804918138375041a9f800f401bec12068/tools/install.sh f423ddfb1d0b6a849b229be5b07a032c10e13c6f &> /dev/null
  }

  function install_yarn {
    if installed yarn; then
      return 0
    fi

    local pkg="yarn"

    # Don't install node too.
    if installed brew; then
      pkg="yarn --ignore-dependencies"
    fi

    if installed apt-get; then
      ensure_apt_add_command

      # Add yarn package provider.
      curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
      echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

      sudo apt-get update
    fi

    install "$pkg"
  }

  function install_ruby {
    if installed ruby; then
      return 0
    fi

    local pkg="ruby"
    if installed apt-get; then
      pkg="ruby-full"
    fi

    install "$pkg"
  }

  function install_tmuxinator {
    if installed tmuxinator; then
      return 0
    fi

    sudo gem install tmuxinator
  }

  function install_silver_searcher {
    if installed ag; then
      return 0
    fi

    local pkg_name="the_silver_searcher"

    # Goes by a different name on aptitude.
    if installed apt-get; then
      pkg_name="silversearcher-ag"
    fi

    install "$pkg_name"
  }

  function install_llama_zsh_theme {
    if [[ -f ~/.oh-my-zsh/themes/llama.zsh-theme ]]; then
      return 0
    fi

    local theme=`curl https://cdn.rawgit.com/PsychoLlama/llama.zsh-theme/29f66554ed63609becbbd60e80f75aa4a8e72c49/llama.zsh-theme`

    if ! verify_hash "803c3c044e238f54ecf91d62c729bc746fe6c0ee" "$theme"; then
      echo "llama zsh theme install failed. The hash doesn't match."
      exit 1
    fi

    echo "$theme" > ~/.oh-my-zsh/themes/llama.zsh-theme
  }

  # A node version manager, alternative to nvm.
  function install_n {
    if installed n; then
      return 0
    fi

    git clone https://github.com/tj/n.git ~/.n-bin &> /dev/null

    pushd ~/.n-bin &> /dev/null
    sudo make &> /dev/null
    popd &> /dev/null

    rm -rf ~/.n-bin
  }

  function install_node {
    if installed node; then
      return 0
    fi

    N_PREFIX=~/.n n latest
  }

  function install_vundle {
    if [[ -d ~/.vim/bundle/Vundle.vim ]]; then
      return 0
    fi

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
  }

  function install_neovim {
    if installed nvim; then
      return 0
    fi

    local pkg="neovim"
    if installed apt-get; then
      local ppa="ppa:neovim-ppa/stable"

      source /etc/os-release
      if [[ "$VERSION" =~ "^14.04" ]]; then
        ppa="ppa:neovim-ppa/unstable"

      fi

      # Add the neovim ppa.
      sudo add-apt-repository "$ppa" <<< "\n"
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

  function install_neovim_plugins {
    if ! python3 -c "import neovim" &> /dev/null; then
      pip3 install neovim &> /dev/null
    fi

    # Source the vimrc in non-interactive mode.
    nvim\
      -u /dev/null\
      +"silent so ~/.config/nvim/init.vim"\
      +PluginInstall +UpdateRemotePlugins +qa
  }

  ensure curl
  ensure openssl
  ensure tmux

  install_make
  install_zsh
  install_oh_my_zsh
  install_yarn
  install_ruby
  install_tmuxinator
  install_silver_searcher
  install_llama_zsh_theme
  install_n
  install_node
  install_vundle
  install_neovim
  install_neovim_plugins
}

main
