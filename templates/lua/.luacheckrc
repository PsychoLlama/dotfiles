ignore = {
  '631', -- max_line_length (managed by stylua)
}

read_globals = {
  'vim',
  'describe',
  'it',
  'assert',
  'before_each',
  'after_each',
  'pending',
}

files['tests'] = {
  ignore = {
    -- Nothing for now.
  },
}
