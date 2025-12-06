local repo = vim.fn.expand('<sfile>:h')

require('core.pkg').add('my-plugin', {
  source = repo,
  type = 'path',
})
