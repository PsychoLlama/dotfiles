--- @brief
--- Support for sourcing vimrc files at runtime using environment variables.
--- This is designed for direnv's project-local vimrc feature. See
--- `direnv-stdlib(1)` for more details.
---
--- For safety, we ask permission before loading unknown scripts.

local permission = require('core.env._permission')

local M = {}

--- Check for a direnv vimrc and if it exists, source the file it points to.
function M.source_direnv_vimrc()
  local env_var = 'DIRENV_EXTRA_VIMRC'
  local vimrc_paths = os.getenv(env_var)

  -- No direnv vimrc defined.
  if not vimrc_paths then
    return
  end

  -- Check permission to load each. RC files can be specified multiple times
  -- by using a colon as a separator. This might happen if you're using
  -- `source_env` or `source_up` from the stdlib.
  for _, path in ipairs(vim.split(vimrc_paths, ':')) do
    M.load(path, {
      prompt = ('Trust extra vimrc? ($%s)'):format(env_var),
    })
  end
end

--- Load a file and ask for permission if it's not already allowed.
--- @param file_path string The file to source.
--- @param opts core.env.PromptOptions
function M.load(file_path, opts)
  if permission.check_memory_or_ask(file_path, opts) == 'allow' then
    local succeeded, message = pcall(vim.cmd.source, file_path)

    -- Multi-line warnings pause the load sequence. `core.env` errors are not
    -- fatal, but they are important and worth raising an alarm.
    if not succeeded then
      vim.notify(
        ('[core.env] Failed to load file:\n%s'):format(message),
        vim.log.levels.WARN
      )
    end
  end
end

return M
