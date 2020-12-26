" Pulled from the tmux man page under the FORMATS section.
let s:variables = [
      \   'alternate_on',
      \   'alternate_saved_x',
      \   'alternate_saved_y',
      \   'buffer_created',
      \   'buffer_name',
      \   'buffer_sample',
      \   'buffer_size',
      \   'client_activity',
      \   'client_created',
      \   'client_control_mode',
      \   'client_discarded',
      \   'client_height',
      \   'client_key_table',
      \   'client_last_session',
      \   'client_name',
      \   'client_pid',
      \   'client_prefix',
      \   'client_readonly',
      \   'client_session',
      \   'client_termname',
      \   'client_termtype',
      \   'client_tty',
      \   'client_utf8',
      \   'client_width',
      \   'client_written',
      \   'command',
      \   'command_list_name',
      \   'command_list_alias',
      \   'command_list_usage',
      \   'cursor_flag',
      \   'cursor_x',
      \   'cursor_y',
      \   'history_bytes',
      \   'history_limit',
      \   'history_size',
      \   'hook',
      \   'hook_pane',
      \   'hook_session',
      \   'hook_session_name',
      \   'hook_window',
      \   'hook_window_name',
      \   'host',
      \   'host_short',
      \   'insert_flag',
      \   'keypad_cursor_flag',
      \   'keypad_flag',
      \   'line',
      \   'mouse_any_flag',
      \   'mouse_button_flag',
      \   'mouse_standard_flag',
      \   'mouse_all_flag',
      \   'pane_active',
      \   'pane_at_bottom',
      \   'pane_at_left',
      \   'pane_at_right',
      \   'pane_at_top',
      \   'pane_bottom',
      \   'pane_current_command',
      \   'pane_current_path',
      \   'pane_dead',
      \   'pane_dead_status',
      \   'pane_format',
      \   'pane_height',
      \   'pane_id',
      \   'pane_in_mode',
      \   'pane_input_off',
      \   'pane_index',
      \   'pane_left',
      \   'pane_mode',
      \   'pane_pid',
      \   'pane_pipe',
      \   'pane_right',
      \   'pane_search_string',
      \   'pane_start_command',
      \   'pane_synchronized',
      \   'pane_tabs',
      \   'pane_title',
      \   'pane_top',
      \   'pane_tty',
      \   'pane_width',
      \   'pid',
      \   'scroll_region_lower',
      \   'scroll_region_upper',
      \   'scroll_position',
      \   'selection_present',
      \   'session_alerts',
      \   'session_attached',
      \   'session_activity',
      \   'session_created',
      \   'session_format',
      \   'session_last_attached',
      \   'session_group',
      \   'session_grouped',
      \   'session_height',
      \   'session_id',
      \   'session_many_attached',
      \   'session_name',
      \   'session_stack',
      \   'session_width',
      \   'session_windows',
      \   'socket_path',
      \   'start_time',
      \   'version',
      \   'window_activity',
      \   'window_activity_flag',
      \   'window_active',
      \   'window_bell_flag',
      \   'window_flags',
      \   'window_format',
      \   'window_height',
      \   'window_id',
      \   'window_index',
      \   'window_last_flag',
      \   'window_layout',
      \   'window_linked',
      \   'window_name',
      \   'window_panes',
      \   'window_silence_flag',
      \   'window_stack_index',
      \   'window_visible_layout',
      \   'window_width',
      \   'window_zoomed_flag',
      \   'wrap_flag',
      \ ]

let s:separator = '<!out:sep@>'

" Maximum arg size to tmux display is 2065 characters.
" Anything past it and the output will be empty.
" Call tmux until all variables have printed.
func! s:get_printable_range(variables, start_index) abort
  let l:index = a:start_index

  let l:cmd = 'tmux display -p "'
  while l:index < len(a:variables)
    let l:feature = a:variables[l:index]
    let l:with_another_feature = l:cmd . s:separator . '#{' . l:feature . '}'

    " Stop if the command is too long.
    if strlen(l:with_another_feature) >= 2064
      break
    endif

    let l:cmd = l:with_another_feature
    let l:index += 1
  endwhile
  let l:cmd .= '"'

  return { 'index': l:index, 'command': l:cmd }
endfunc

func! s:get_var_print_commands(variables) abort
  let l:commands = []
  let l:prefix = 'tmux display -p "'
  let l:index = 0

  while l:index < len(a:variables)
    let l:result = s:get_printable_range(a:variables, l:index)
    let l:index = l:result.index
    call add(l:commands, l:result.command)
  endwhile

  return l:commands
endfunc

func! s:get_variables(variables) abort
  let l:cmds = s:get_var_print_commands(a:variables)
  let l:result = []

  for l:cmd in l:cmds
    let l:output = editor#util#chomp(system(l:cmd))
    let l:result += split(l:output, s:separator)
  endfor

  return l:result
endfunc

func! tmux#get_variables() abort
  let l:result = {}
  let l:variables = s:get_variables(s:variables)
  let l:index = 0

  while l:index < len(l:variables)
    let l:var_name = s:variables[l:index]
    let l:var = l:variables[l:index]
    let l:result[l:var_name] = l:var
    let l:index += 1
  endwhile

  return l:result
endfunc

func! tmux#split_window(...) abort
  let l:config = get(a:000, 0, {})
  let l:horizontal = get(l:config, 'horizontal', v:false)
  let l:percent = get(l:config, 'percent', 0)

  let l:cmd = 'tmux split-window'
  if l:horizontal
    let l:cmd .= ' -h'
  endif

  if l:percent > 0
    let l:cmd .= ' -p ' . l:percent
  endif

  call system(l:cmd)
  return system('tmux display -p "#{pane_id}"')
endfunc

func! tmux#send_keys(...) abort
  let l:keys = map(copy(a:000), { idx, str -> escape(str, '"') })
  let l:keys = map(l:keys, "'\"' . v:val . '\"'")
  let l:keys = join(l:keys, ' ')

  call system('tmux send-keys ' . l:keys)
endfunc

" By index (e.g. 1, 2, 3) or by ID (e.g. %20, %16)
func! tmux#select_pane(id) abort
  call system('tmux select-pane -t ' . a:id)
endfunc
