# Adapted from my old llama.zsh-theme prompt.
function fish_prompt
  set -l last_exit_code $status
  set -l is_git_repo (
    git rev-parse --is-inside-work-tree 2> /dev/null
    or echo 'false'
  )

  function _directory_name
    basename $PWD
  end

  # Tries to find a human-readable branch name like "dev" or "master".
  # Falls back to a 7-character hash.
  function _current_git_branch
    set -l branch (git rev-parse --abbrev-ref HEAD)

    if test $branch = 'HEAD'
      git rev-parse HEAD | cut -c 1-7
    else
      echo -n $branch
    end
  end

  # The prompt icon is red for non-zero output.
  function _exit_status -S
    if test $last_exit_code = 0
      set_color green
    else
      set_color red
    end

    # Note: this icon assumes you're using the "Hack" font.
    echo -n '❯'
  end

  ### Formatting ###
  function _fmt_current_directory
    set_color blue
    echo -n (_directory_name)
  end

  # Prints "[branch_name]" or nothing.
  function _fmt_git_branch -S
    if test $is_git_repo = 'true'
      set_color yellow
      echo -n '['
      set_color cyan
      echo -n (_current_git_branch)
      set_color yellow
      echo -n ']'
    else
      echo ' '
    end
  end

  echo (_fmt_current_directory)(_fmt_git_branch)(_exit_status)' '
end
