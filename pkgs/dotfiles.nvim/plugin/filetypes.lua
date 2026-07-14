vim.filetype.add({
  extension = {
    nomad = 'hcl',
    mdx = 'markdown.mdx',
    ncl = function(_, _)
      vim.bo.commentstring = '#%s'
      return 'nickel'
    end,
    nu = function(_, _)
      vim.bo.commentstring = '#%s'
      return 'nu'
    end,
    nix = function(_, bufnr)
      vim.bo[bufnr].commentstring = '#%s'
      return 'nix'
    end,
    go = function(_, bufnr)
      vim.bo[bufnr].expandtab = false
      return 'go'
    end,
  },
  filename = {
    ['.eslintrc'] = 'json',
    ['.babelrc'] = 'json',
    ['go.mod'] = function(_, bufnr)
      vim.bo[bufnr].expandtab = false
      return 'gomod'
    end,
  },
})
