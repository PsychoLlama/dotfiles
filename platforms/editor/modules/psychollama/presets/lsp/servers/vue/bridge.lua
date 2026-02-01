-- Bridge handler to forward tsserver requests from Vue LSP to vtsls.
-- Required for hybrid mode Vue + TypeScript support.

local function create_tsserver_handler(vue_client)
  local retries = 0
  return function(_, result, context)
    local ts_client = vim.lsp.get_clients({
      bufnr = context.bufnr,
      name = 'vtsls',
    })[1]

    if not ts_client then
      if retries <= 10 then
        retries = retries + 1
        vim.defer_fn(function()
          vue_client.handlers['tsserver/request'](_, result, context)
        end, 100)
      end
      return
    end

    local param = result[1]
    local id, command, payload = param[1], param[2], param[3]
    ts_client:exec_cmd({
      title = 'vue_tsserver_request',
      command = 'typescript.tsserverRequest',
      arguments = { command, payload },
    }, { bufnr = context.bufnr }, function(_, response)
      vue_client:notify(
        'tsserver/response',
        { { id, response and response.body } }
      )
    end)
  end
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('vue.tsserver_bridge', {}),
  callback = function(event)
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client.name == 'vue' then
      client.handlers['tsserver/request'] = create_tsserver_handler(client)
    end
  end,
})
