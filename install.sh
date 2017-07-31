#!/usr/bin/env bash -e

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
  elif installed zypper; then
    INSTALL_CMD="sudo zypper install"
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

    if [[ "$integrity" != "$1" ]]; then
      return 1
    fi

    return 0
  }

  # Install to a /tmp file and verify the integrity hash.
  function install_via_curl {
    local install_script=`curl -fsSL "$1"`

    if verify_hash "$2" "$install_script"; then
      echo "Hmmm, an install script looks sketchy. The integrity doesn't match."
      echo "    URL: $1"
      exit 1
    fi

    eval "$install_script"
  }

  function install_zsh {
    if installed zsh; then
      return 0
    fi

    install zsh
    chsh -s zsh
    zsh
  }

  function install_oh_my_zsh {
    if [[ -d ~/.oh-my-zsh ]]; then
      return 0
    fi

    install_via_curl https://cdn.rawgit.com/robbyrussell/oh-my-zsh/d848c94804918138375041a9f800f401bec12068/tools/install.sh f423ddfb1d0b6a849b229be5b07a032c10e13c6f
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

    git clone https://github.com/tj/n.git /tmp/n-bin

    pushd /tmp/n-bin &> /dev/null
    make
    popd &> /dev/null

    rm -r /tmp/n-bin
  }

  function install_node {
    if installed node; then
      return 0
    fi

    n latest
  }

  ensure curl
  ensure openssl
  ensure yarn
  ensure tmux

  install_zsh
  install_oh_my_zsh
  install_silver_searcher
  install_llama_zsh_theme
  install_n
  install_node
}

main
