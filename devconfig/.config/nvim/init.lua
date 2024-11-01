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

-- Utilize lazy.nvim plugin manager
--local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
--if not vim.loop.fs_stat(lazypath) then
--  vim.fn.system({
--    "git",
--    "clone",
--    "--filter=blob:none",
--    "https://github.com/folke/lazy.nvim.git",
--    "--branch=stable", -- latest stable release
--    lazypath,
--  })
--end
--vim.opt.runtimepath:prepend(lazypath)

-- Define desired plugins
local plugins = {
  -- mason.nvim
  {
    'williamboman/mason.nvim',
    opts = {},
    config = function()
      local mymason = require('mason')
      mymason.setup()
    end
  }

  -- lspconfig.nvim
  ,{
    'neovim/nvim-lspconfig',
    opts = {},
    config = function ()
      mylspconfig = require('lspconfig')
      mylspconfig.clangd.setup{}
      mylspconfig.rust_analyzer.setup{}
    end
  }

  -- nvim-treesitter/nvim-treesitter
  --,{
  --  'nvim-treesitter/nvim-treesitter',
  --  opts = {},
  --  config = function()
  --    vim.cmd('TSUpdate')
  --  end
  --}

  -- mason-lspconfig.nvim
  --,{
  --  'williamboman/mason-lspconfig.nvim'
  --}

  -- rcarriga/nvim-dap-ui
  --,{
  --  'rcarriga/nvim-dap-ui'
  --}

  -- mfussenegger/nvim-lint
  --,{
  --  'mfussenegger/nvim-lint'
  --}

  -- mhartington/formatter.nvim
  --,{
  --  'mhartington/formatter.nvim'
  --}

  -- ale
  ,{
    'dense-analysis/ale',
    opts = {},
    config = function()
      -- Do nothing
    end
  }

  -- nvim-dap
  ,{
    'mfussenegger/nvim-dap',
    opts = {},
    config = function()
      -- Do nothing
    end
  }

  -- oil.nvim
  ,{
    'stevearc/oil.nvim',
    opts = {},
    -- Optional dependencies
    dependencies = {"nvim-tree/nvim-web-devicons"},
    config = function ()
      require("oil").setup()
    end
  }

  -- leap.nvim
  ,{
    'ggandor/leap.nvim',
    opts = {},
    config = function()
      require('leap').create_default_mappings()
    end
  }

  -- vimwiki
  ,{
    'vimwiki/vimwiki',
    opts = {},
    config = function()
      -- Do nothing
    end
  }
}

-- Invoke lazy.nvim to setup desired plugins
--require("lazy").setup(plugins, {})

-- Old sample code to set up LSP client/server
-- vim.lsp.start({
--   name = 'my-server-name',
--   cmd = {'clangd'},
--   root_dir = "/home/zzapotheosis/Development/Git/Personal-Projects/coding_practice/C"
-- })

-- vim.api.nvim_create_autocmd('LspAttach', {
--   callback = function(args)
--     vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = args.buf })
--   end,
-- })
