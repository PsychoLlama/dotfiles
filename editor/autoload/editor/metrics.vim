let s:metrics_filename = expand('~/.vim/metrics.json')
let s:default_state = {
      \     'mappings': {},
      \     'events': {},
      \   }

" Read all metrics into a json file.
func! editor#metrics#Read() abort
  if !filereadable(s:metrics_filename)
    call writefile([json_encode(s:default_state)], s:metrics_filename)
  endif

  let l:contents = readfile(s:metrics_filename)

  return json_decode(l:contents)
endfunc

" Write the entire metrics table.
func! editor#metrics#Write(metrics) abort
  let l:contents = json_encode(a:metrics)

  call writefile([l:contents], s:metrics_filename)
endfunc

" Add an entry to the events set.
func! editor#metrics#TrackEvent(event_name, metadata) abort
  let l:metrics = editor#metrics#Read()

  if !has_key(l:metrics.events, a:event_name)
    let l:metrics.events[a:event_name] = []
  endif

  let l:metric = extend({ 'time': localtime() }, a:metadata, 'error')
  let l:metrics.events[a:event_name] += [l:metric]

  call editor#metrics#Write(l:metrics)
endfunc
