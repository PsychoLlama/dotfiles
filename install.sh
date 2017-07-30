#!/usr/bin/env bash -e

function main {

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

main
