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

  function install_zsh {
    if installed zsh; then
      return 0
    fi

    install zsh
    chsh -s zsh
    zsh
  }

  function install_oh_my_zsh {
    if [[ ! -z "$ZSH" ]]; then
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
    if has apt-get; then
      pkg_name="$pkg_name-ag"
    fi

    install "$pkg_name"
  }

  ensure curl
  ensure openssl

  install_zsh
  install_oh_my_zsh
  install_silver_searcher
}

main
