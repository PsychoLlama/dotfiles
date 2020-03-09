function _adjust_directory_frecency --on-event fish_prompt
  zoxide add
end

function z
  if test (count $argv) -eq 0
    return 1
  end

  set result (zoxide query $argv[1] | sed 's/query: //')

  if test -z $result
    return 1
  end

  cd $result
end
