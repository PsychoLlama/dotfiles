ignore = {
  '631', -- max_line_length (managed by stylua)
}

include_files = {
  'pkgs/**/*.lua',
  'platforms/**/*.lua',
}

exclude_files = {
  'result*/**',
}

globals = {
  'vim',
}

read_globals = {
  'describe',
  'it',
  'assert',
  'before_each',
  'after_each',
  'pending',
}
