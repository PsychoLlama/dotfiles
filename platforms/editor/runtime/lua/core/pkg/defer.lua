--- @brief
--- Lazy-loading utilities for core.pkg. Allows deferring plugin loading and
--- configuration until specific events, commands, or keymaps trigger them.

local M = {}

--- @class core.pkg.DeferSpec
--- @field event? string|string[] Autocmd event(s) to trigger loading
--- @field pattern? string|string[] Pattern for event matching (e.g., filetype)
--- @field cmd? string|string[] Command(s) that trigger loading
--- @field keys? string|string[]|core.pkg.KeySpec[] Keymap(s) that trigger loading
--- @field ft? string|string[] Filetype(s) that trigger loading (shorthand for FileType event)

--- @class core.pkg.KeySpec
--- @field [1] string The key sequence
--- @field mode? string|string[] Mode(s) for the keymap (default: 'n')
--- @field desc? string Description for the keymap

--- Registry of deferred plugins
--- @type table<string, { loaded: boolean, spec: core.pkg.DeferSpec, plugin: core.pkg.Plugin }>
local deferred = {}

--- Track cleanup functions for removing temporary triggers
--- @type table<string, function[]>
local cleanups = {}

--- Load a deferred plugin and run its config
--- @param name string Plugin name
local function load_deferred(name)
  local entry = deferred[name]
  if not entry or entry.loaded then
    return
  end

  entry.loaded = true

  -- Clean up triggers
  if cleanups[name] then
    for _, cleanup in ipairs(cleanups[name]) do
      cleanup()
    end
    cleanups[name] = nil
  end

  -- Load the plugin
  local plugin = entry.plugin
  if plugin.type == 'pack' then
    vim.cmd.packadd(plugin.name)
  else
    vim.opt.runtimepath:prepend(plugin.source)
    local after = vim.fs.joinpath(plugin.source, 'after')
    if vim.fn.isdirectory(after) == 1 then
      vim.opt.runtimepath:append(after)
    end
  end

  -- Run the config
  if plugin.config then
    local callback = plugin.config
    if type(callback) == 'string' then
      callback = dofile(callback)
    end
    if type(callback) == 'function' then
      callback(plugin.opts)
    end
  end
end

--- Register a temporary command that loads the plugin on first use
--- @param name string Plugin name
--- @param cmd string Command name
local function register_command_trigger(name, cmd)
  cleanups[name] = cleanups[name] or {}

  -- Create a temporary command that loads the plugin and re-executes
  vim.api.nvim_create_user_command(cmd, function(opts)
    load_deferred(name)
    -- Re-execute the command now that the real one is loaded
    local args = opts.args ~= '' and (' ' .. opts.args) or ''
    vim.cmd(cmd .. args)
  end, {
    nargs = '*',
    complete = function()
      load_deferred(name)
      -- Return completions from the real command
      return vim.fn.getcompletion(cmd .. ' ', 'cmdline')
    end,
  })

  table.insert(cleanups[name], function()
    pcall(vim.api.nvim_del_user_command, cmd)
  end)
end

--- Register a temporary keymap that loads the plugin on first use
--- @param name string Plugin name
--- @param key string|core.pkg.KeySpec Key specification
local function register_key_trigger(name, key)
  cleanups[name] = cleanups[name] or {}

  local lhs, modes, desc
  if type(key) == 'string' then
    lhs = key
    modes = { 'n' }
    desc = 'Load ' .. name
  else
    lhs = key[1]
    modes = type(key.mode) == 'string' and { key.mode }
      or (key.mode or { 'n' })
    desc = key.desc or ('Load ' .. name)
  end

  for _, mode in
    ipairs(modes --[[@as string[] ]])
  do
    vim.keymap.set(mode, lhs, function()
      load_deferred(name)
      -- Re-trigger the keymap using lazy.nvim's approach:
      -- - Prepend <Ignore> so Neovim processes the key transparently
      -- - Use 'i' mode to insert at front of typeahead buffer
      local feed = vim.api.nvim_replace_termcodes('<Ignore>' .. lhs, true, true, true)
      vim.api.nvim_feedkeys(feed, 'i', false)
    end, { desc = desc })

    table.insert(cleanups[name], function()
      pcall(vim.keymap.del, mode, lhs)
    end)
  end
end

--- Register an autocmd that loads the plugin on event
--- @param name string Plugin name
--- @param event string|string[] Event(s)
--- @param pattern? string|string[] Pattern(s)
local function register_event_trigger(name, event, pattern)
  cleanups[name] = cleanups[name] or {}

  local group =
    vim.api.nvim_create_augroup('core_pkg_defer_' .. name, { clear = true })

  vim.api.nvim_create_autocmd(event, {
    group = group,
    pattern = pattern or '*',
    once = true,
    callback = function()
      load_deferred(name)
    end,
  })

  table.insert(cleanups[name], function()
    pcall(vim.api.nvim_del_augroup_by_id, group)
  end)
end

--- Defer a plugin's loading until triggers fire
--- @param plugin core.pkg.Plugin The plugin to defer
--- @param spec core.pkg.DeferSpec Defer specification
function M.register(plugin, spec)
  local name = plugin.name
  deferred[name] = {
    loaded = false,
    spec = spec,
    plugin = plugin,
  }

  -- Register command triggers
  if spec.cmd then
    local cmds = type(spec.cmd) == 'string' and { spec.cmd } or spec.cmd
    for _, cmd in
      ipairs(cmds --[[@as string[] ]])
    do
      register_command_trigger(name, cmd --[[@as string]])
    end
  end

  -- Register key triggers
  if spec.keys then
    local keys = spec.keys
    if type(keys) == 'string' then
      keys = { keys }
    end
    for _, key in
      ipairs(keys --[[@as (string|core.pkg.KeySpec)[] ]])
    do
      register_key_trigger(name, key)
    end
  end

  -- Register filetype triggers (shorthand for FileType event)
  if spec.ft then
    local fts = type(spec.ft) == 'string' and { spec.ft } or spec.ft
    register_event_trigger(name, 'FileType', fts)
  end

  -- Register event triggers
  if spec.event then
    register_event_trigger(name, spec.event, spec.pattern)
  end
end

--- Check if a plugin is deferred
--- @param name string Plugin name
--- @return boolean
function M.is_deferred(name)
  return deferred[name] ~= nil
end

--- Check if a deferred plugin has been loaded
--- @param name string Plugin name
--- @return boolean
function M.is_loaded(name)
  local entry = deferred[name]
  return entry and entry.loaded or false
end

--- Force load a deferred plugin
--- @param name string Plugin name
function M.load(name)
  load_deferred(name)
end

--- Get list of all deferred plugins
--- @return string[]
function M.list()
  return vim.tbl_keys(deferred)
end

return M
