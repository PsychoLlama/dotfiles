### Setup ###
plugins=(git z docker vagrant tmux)
ZSH_THEME='llama'


### Variables ###
export N_PREFIX=~/.n
export PATH=$N_PREFIX/bin:$PATH
export PATH=$(yarn global bin):$PATH
export ZSH=~/.oh-my-zsh
export EDITOR=nvim
export PORT=8080
export TERM=xterm-256color


### Aliases ###
alias sudo='sudo '
alias D='cd ~/Desktop'
alias empty='empty-trash'
alias todo='task'
alias :q='exit'
alias :qa='tmux kill-session'
alias v='nvim -p'
alias t='tmux'
alias ch='git checkout'
alias b='git branch --verbose'
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
  `dotfiles dir`/utils/get-package-test-script.js | xargs yarn
}

function gag {
  ag $@\
    --ignore node_modules\
    --ignore '*.bundle.*'\
    --ignore schema.js\
    --ignore dist\
    --ignore coverage
}

function vs {
  nvim -p `gag $@ -l`
}


# Kickstart the oh-my-zsh framework.
source $ZSH/oh-my-zsh.sh


# Not all functions and aliases should be shared.
if [[ -e ~/.custom-scripts/.zshrc ]]; then
  source ~/.custom-scripts/.zshrc
fi
