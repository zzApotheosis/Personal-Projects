-- Append package load path
package.path = package.path .. ';' .. os.getenv('HOME') .. '/.config/nvim/?.lua'

-- Load version check module
local versioncheck = require('init.versioncheck')

-- Check version first before anything else
if versioncheck.versioncheck() < 0 then
  print("This Neovim version is too old! (Minimum " .. versioncheck.expected() .. ', actual ' .. versioncheck.actual() .. ')')
  vim.fn.input("Press Enter to continue")
  os.exit(1)
end

-- Initialize setup (All custom modules must exist under $HOME/.config/nvim/?.lua)
require('init.vimoptions').init()
require('init.colors').init()
require('init.lsp').init()
require('init.usercommands').init()
require('init.autocommands').init()
