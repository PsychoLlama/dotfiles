#!/usr/bin/env fish
set -x VIRTUAL_ENV_DISABLE_PROMPT 1
set -x SKIM_DEFAULT_COMMAND fd
set -x COLORTERM truecolor
set -x EDITOR nvim
set -x --prepend PATH ~/.cargo/bin
set -x PAGER 'bat --paging=always --tabs=2'
set -x MANPAGER 'nvim -c "setfiletype man" -'
set -x RIPGREP_CONFIG_PATH ~/.ripgrep
set -x BAT_STYLE changes
set -x NIX_IGNORE_SYMLINK_STORE 1 # Catalina ruins everything.
set -x --prepend PATH ~/.nix-profile/bin

if test -d ~/.fnm
  set -gx --prepend PATH ~/.fnm
  set -gx --prepend PATH ~/.fnm/current/bin
  set -gx FNM_DIR ~/.fnm/
  set -gx FNM_NODE_DIST_MIRROR 'https://nodejs.org/dist'
end

alias g git
alias b 'git --no-pager branch --verbose'
alias :qa 'tmux kill-session 2> /dev/null; or exit'
alias ch 'git checkout'
alias D 'cd ~/Desktop'
alias c 'git commit'
alias H 'git diff HEAD'
alias HH 'git diff HEAD~1'
alias HHH 'git diff HEAD~2'
alias l 'ls -lAh'

alias ... 'cd ../../'
alias .... 'cd ../../../'

# Git branch "dirstack"
for num in (seq 1 9)
  alias b{$num} "git checkout (git branch-history $num | tail -1)"
end

### Functions ###
function mkcd
  if test (count $argv) -lt 1
    echo 'And you failed.'
    return 1
  end

  mkdir -p $argv[1]
  cd $argv[1]
end

# Open a development environment in the current directory
function edit
  set -l session_name $argv[1]

  if test -z "$session_name"
    set session_name (basename $PWD)
  end

  tmuxinator start edit -n $session_name
end

# Faster `git status`
function s
  if not git rev-parse --is-inside-work-tree > /dev/null 2>&1
    echo "Not a git repo."
    return 1
  end

  git status
end

# tmux shorthand
function t
  # Arguments? Treat `t` like an alias.
  if test (count $argv) -gt 0
    tmux $argv
    return $status
  end

  if test -n "$TMUX"
    echo "Chill out dude. One tmux is enough." >&2
    return 1
  end

  # Attach to the most recent session, or create one.
  set -l recent_session (tmux ls 2> /dev/null | awk 'END {print $1}' | sed 's/://')

  if test -z "$recent_session"
    tmux new
    return $status
  end

  tmux attach -t $recent_session
  return $status
end

# tmux attach with skim
function ta
  set -l session (tmux list-sessions -F '#{session_name}' | sk)

  if test -z "$session"
    return 1
  end

  if test -z "$TMUX"
    tmux attach -t "$session"
  else
    tmux switch-client -t "$session"
  end
end

function gg
  rg $argv --ignore-file <(echo "__tests__\ntests\nflow-typed")
end

# Like `nvim` but better
function v
  if test (count $argv) = 0
    nvim .
  else
    nvim -p $argv
  end
end

function vs
  if test (count $argv) = 0; then
    echo "WRONG! Try again."
    return 1
  end

  nvim -p (gg $argv -l)
end

function vh
  if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1
    echo 'Not a repo, loser.'
    return 1
  end

  set -l repo_root (git rev-parse --show-toplevel)
  set -l filepaths

  git diff HEAD --name-only | while read filepath
    set --append filepaths "$repo_root/$filepath"
  end

  if test (count filepaths) = 0; then
    echo 'No changes :/'
    return 1
  end

  nvim -p $filepaths
end

function vf
  set -l cache_file (dotfiles dir)/command-cache/vf-last-result

  if test (count $argv) = 0
    set file (sk)

    if test -z "$file"
      return 1
    end

    echo "$PWD/$file" > $cache_file
    echo $file
    nvim $file

    return $status
  end

  switch $argv[1]
    case '--last' '-l'
      set file (cat $cache_file)

      if test -z "$file"
        echo "There's no find history :/" >&2
        return 1
      end

      nvim $file
      return $status
  case *
    echo "{$argv[1]}? You sure about that?"
    return 1
  end
end

# TODO: move this into its own repro.
source (dotfiles dir)/prompt.fish

# Check for computer-specific shell utils.
if test -e ~/dotfiles-env/config.fish
  source ~/dotfiles-env/config.fish
end
