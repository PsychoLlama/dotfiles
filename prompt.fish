# Adapted from my old llama.zsh-theme prompt.
function fish_prompt
  set -l last_exit_code "$status"
  set -l is_git_repo (
    git rev-parse --is-inside-work-tree 2> /dev/null
    or echo 'false'
  )

  set -l show_python_info (
    test -n "$VIRTUAL_ENV_DISABLE_PROMPT" -a -n "$VIRTUAL_ENV"
    and echo 'true'
    or echo 'false'
  )

  function _directory_name
    basename "$PWD"
  end

  #############
  #    Git    #
  #############

  # Tries to find a human-readable branch name like "dev" or "master".
  # Falls back to a 7-character hash.
  function _current_git_branch
    set -l branch (git rev-parse --abbrev-ref HEAD)

    if test "$branch" = 'HEAD'
      git rev-parse HEAD | cut -c 1-7
    else
      echo -n "$branch"
    end
  end

  # Show a colored summary of the git status in a '+' symbol.
  function _git_status -S
    if test "$is_git_repo" = 'false'
      return
    end

    set -l staged_changes (git diff --name-only --cached)
    set -l unstaged_changes (git ls-files --others --modified --exclude-standard)

    if test -z "$staged_changes" -a -z "$unstaged_changes"
      return
    end

    if test -n "$unstaged_changes" -a -n "$staged_changes"
      set_color yellow
    else if test -n "$unstaged_changes"
      set_color red
    else if test -n "$staged_changes"
      set_color green
    end

    echo -n '+'
  end

  ##############
  #    Venv    #
  ##############
  function _fmt_python_version -S
    if test "$show_python_info" = 'false'
      return 1
    end

    set_color yellow
    echo -n '('
    set_color cyan
    echo -n (python --version | sed -e 's/Python //' -e 's/\\.[0-9]*$//')
    set_color yellow
    echo -n ')'
  end

  ##############
  #    Misc    #
  ##############

  # The prompt icon is red for non-zero output.
  function _exit_status -S
    if test "$last_exit_code" = 0
      set_color green
    else
      set_color red
    end

    # Note: this icon assumes you're using the "Hack" font.
    echo -n '❯'
  end

  function _fmt_current_directory
    set_color blue
    echo -n (_directory_name)
  end

  # Prints "[branch_name]" or nothing.
  function _fmt_git_branch -S
    if test "$is_git_repo" = 'true'
      set_color yellow
      echo -n '['
      echo -n (_git_status)
      set_color cyan
      echo -n (_current_git_branch)
      echo -n (_fmt_python_version)
      set_color yellow
      echo -n ']'
    else
      echo -n (_fmt_python_version; or echo -n ' ')
    end
  end

  echo (_fmt_current_directory)(_fmt_git_branch)(_exit_status)' '
end
