--- @brief
--- Support for sourcing vimrc files at runtime using environment variables.
--- This is designed for direnv's project-local vimrc feature. See
--- `direnv-stdlib(1)` for more details.
---
--- For safety, we ask permission before loading unknown scripts.

local permission = require('core.env._permission')

local M = {}

--- Check for an environment variable and if it exists, source the file it
--- points to.
---
--- @param env_var string Env var of the custom vimrc filepath.
function M.load_from_env(env_var)
  local project_vimrc = os.getenv(env_var)

  -- No vimrc defined.
  if not project_vimrc then
    return
  end

  return M.load(project_vimrc, {
    prompt = ('Load extra vimrc? ($%s)'):format(env_var),
  })
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
