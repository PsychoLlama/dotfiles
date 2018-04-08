" Locate the directory defining package.json.
function! editor#js#FindPackageRoot() abort
  let l:current_dir = expand('%:p:h')
  let l:HasPkgJson = {dir -> file_readable(dir . '/package.json')}
  let l:project = editor#util#SearchDirUpwards(l:current_dir, l:HasPkgJson)

  if l:project is v:null
    let l:project = l:current_dir
  endif

  return l:project
endfunction

" Find the current file by searching (../)*__tests__/%:p:h:r.test.js
function! editor#js#LocateTestFile() abort
  let l:current_dir = expand('%:p:h')
  let l:HasTestDir = {dir -> isdirectory(dir . '/__tests__')}
  let l:test_dir = editor#util#SearchDirUpwards(l:current_dir, l:HasTestDir)

  if l:test_dir is v:null
    return v:null
  endif

  let l:test_file = l:test_dir . '/__tests__/' . expand('%:p:t:r') . '.test.js'
  return l:test_file
endfunction
