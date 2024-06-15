local M = {}

function M.validate(file_path)
  if vim.fn.filereadable(file_path) == 0 then
    vim.notify('Wait, are you sure this is a file?', vim.log.levels.WARN)
    return false
  end

  return true
end

function M.get(file_path)
  if not M.validate(file_path) then
    return
  end

  return vim.fn.getfperm(file_path)
end

function M.set(file_path, perms)
  if not M.validate(file_path) then
    return
  end

  -- Using `chmod` because it supports named permissions.
  local job = vim.system(
    { 'chmod', perms, vim.fn.fnameescape(file_path) },
    { text = true }
  ):wait()

  if job.code ~= 0 then
    vim.notify('Failed: ' .. job.stderr, vim.log.levels.ERROR)
  end
end

function M.command(ctx)
  local file_path = vim.fn.expand('%:p')

  if not M.validate(file_path) then
    return
  end

  if #ctx.fargs == 1 then
    M.set(file_path, ctx.fargs[1])
  end

  vim.print(M.get(file_path))
end

return M
