-- Module definition
local usercommands = {}

-- Setup commands
function usercommands.setup()
  -- Include modules
  local lsp = require('init.lsp')
  local versioncheck = require('init.versioncheck')

  -- Create custom commands
  vim.api.nvim_create_user_command('LSPStop', lsp.stop_all_lsp, {})
  vim.api.nvim_create_user_command('VersionCheck', versioncheck.versioncheck, {})
end

-- Init function
function usercommands.init()
  usercommands.setup()
end

-- Export module
return usercommands
