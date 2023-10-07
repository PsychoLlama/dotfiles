return {
  go_to_last_line = function()
    vim.fn.cursor(vim.fn.line('$'), 1)
  end,

  normalize_title = function(title)
    return vim.fn.tolower(vim.fn.tr(title, ' :', '--'))
  end,
}
