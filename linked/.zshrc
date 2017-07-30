### Setup ###
plugins=(git z docker vagrant tmux)
ZSH_THEME='llama'


### Variables ###
export N_PREFIX=~/.n
export PATH=$N_PREFIX/bin:$PATH
export PATH=$(yarn global bin):$PATH
export ANDROID_HOME=/usr/local/opt/android-sdk
export ZSH=~/.oh-my-zsh
export EDITOR=nvim
export PORT=8080


### Tools ###
source ~/.cargo/env
source ~/.travis/travis.sh
source $ZSH/oh-my-zsh.sh


### Aliases ###
alias sudo='sudo '
alias D='cd ~/Desktop'
alias empty='empty-trash'
alias todo='task'
alias :q='exit'
alias :qa='tmux kill-session'
alias v='nvim'
alias t='tmux'
alias ch='git checkout'
alias b='git branch'
alias c='git commit'


### Functions ###
function mkcd {
  mkdir -p "$1"
  cd "$1"
}

function edit {
  local session_name=$(basename ${PWD})
  tmuxinator start edit -n ${session_name}
}

function remote {
  local dir=${PWD}
  cd ~

  if [[ "$1" == "up" ]]; then
    vagrant up
  elif [[ "$1" == "down" ]]; then
    vagrant halt
    return $?
  fi

  vagrant ssh krikkit -c 'tmuxinator start HireVue'
  cd $dir
}

function s {
  $(git rev-parse --is-inside-work-tree 2&> /dev/null)

  if [[ $? != 0 ]]; then
    echo "Not a git repo."
    return 1
  else
    git status
  fi
}

function tdd {
  ~/get-test-script.js | xargs yarn
}
