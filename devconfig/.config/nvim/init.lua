-- Expected Neovim version
local min_expected = {}
min_expected['major'] = 0
min_expected['minor'] = 8
min_expected['patch'] = 0

-- Set standard Neovim configuration options
vim.opt.number = true
-- vim.opt.relativenumber = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.ai = true
vim.opt.si = true
vim.opt.cindent = true
vim.opt.mouse = 'a'
vim.opt.foldmethod = 'indent'

-- Set transparent background no matter what
vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })

--vim.opt.background = 'none'
--vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
--vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

-- Custom function to check running version
function version_check()
  local actual = vim.version()

  if actual['major'] < min_expected['major'] then
    return -1
  elseif actual['major'] > min_expected['major'] then
    return 1
  end

  if actual['minor'] < min_expected['minor'] then
    return -1
  elseif actual['minor'] > min_expected['minor'] then
    return 1
  end

  if actual['patch'] < min_expected['patch'] then
    return -1
  elseif actual['patch'] > min_expected['patch'] then
    return 1
  end

  return 0
end
vim.api.nvim_create_user_command('Test', version_check, {})

function stop_lsp()
  vim.lsp.stop_client(vim.lsp.get_active_clients())
end

-- Set up autocmd for LSP servers
vim.api.nvim_create_autocmd('FileType', {
  pattern = {'c', 'cpp', 'c++'},
  callback = function(args)
    vim.lsp.start({
      name = 'clangd',
      cmd = {'clangd'},
    })
  end,
})
vim.api.nvim_create_autocmd('FileType', {
  pattern = {'zig'},
  callback = function(args)
    vim.lsp.start({
      name = 'zls',
      cmd = {'zls'},
    })
  end,
})
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'rust',
  callback = function(args)
    vim.lsp.start({
      name = 'rust-analyzer',
      cmd = {'rust-analyzer'},
      root_dir = vim.fs.root(args.buf, {'Cargo.toml'}),
    })
  end,
})

-- Create custom commands
vim.api.nvim_create_user_command('LSPStop', stop_lsp, {})

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
