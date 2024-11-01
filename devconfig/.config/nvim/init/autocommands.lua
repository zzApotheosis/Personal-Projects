-- Module definition
local autocommands = {}
local m = {}

-- Setup function
function m.setup()
  vim.api.nvim_create_autocmd('VimEnter', {
    callback = function(args)
      local clients = vim.lsp.get_clients()
      for _, client in ipairs(clients) do
        vim.lsp.completion.enable(true, client.id, { autotrigger = true })
        return -- is this actually needed?
      end
    end,
  })
end

-- Init function
function autocommands.init()
  m.setup()
end

-- Export module
return autocommands
