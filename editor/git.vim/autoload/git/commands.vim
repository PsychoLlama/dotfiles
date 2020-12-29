" Order authors by quantity of lines written.
func! s:get_author_ownership(authors, total) abort
  let l:result = []

  for l:author in keys(a:authors)
    let l:owned_lines = a:authors[l:author]
    let l:percentage = (l:owned_lines / a:total) * 100
    let l:result += [[l:author, float2nr(round(l:percentage))]]
  endfor

  call sort(l:result, {pair1, pair2 -> pair2[1] - pair1[1]})

  return l:result
endfunc

func! s:find_authors_for_range(start, end, all_commits) abort
  let l:file = resolve(expand('%:p'))
  let l:line_blames = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:start, a:end]],
        \   'file': l:file,
        \ })

  let l:authors = map(l:line_blames, { k, v -> v.author.name })
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:uniq_authors[l:author] = get(l:uniq_authors, l:author, 0) + 1
  endfor

  return s:get_author_ownership(l:uniq_authors, a:end - a:start + 1.0)
endfunc

func! s:print_line_details(line, all_commits) abort
  let [l:details] = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:line, a:line]],
        \   'file': expand('%:p'),
        \ })

  let l:date = strftime('%m/%d/%Y', l:details.author.time)
  echohl String
  echon l:details.sha[0:6]
  echohl Clear
  echon ': ' . l:details.author.name . ' ('
  echohl Type
  echon l:date
  echohl Clear
  echon ')'

  echo l:details.summary
endfunc


" :<range>Author[!]
func! git#commands#author(start, end, all_commits) abort
  if &modified
    echo 'Save your changes first.'
    return
  endif

  if isdirectory(expand('%:p'))
    echo 'Uh, this is a directory'
    return
  endif

  " If there's only one selected line, show more details.
  if a:start == a:end
    return s:print_line_details(a:start, a:all_commits)
  endif

  for [l:author, l:ownership] in s:find_authors_for_range(a:start, a:end, a:all_commits)
    echohl Clear
    echo l:author
    echohl Comment
    echon ' (' . l:ownership . '%)'
    echohl Clear
  endfor
endfunc
