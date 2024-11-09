-- Module definition
local lazyvim = {}
local m = {}

-- Define desired plugins
m.plugins = {
  -- mason.nvim
  --{
  --  'williamboman/mason.nvim',
  --  opts = {},
  --  config = function()
  --    local mymason = require('mason')
  --    mymason.setup()
  --  end
  --}

  -- nvim-lspconfig
  --{
  --  "neovim/nvim-lspconfig", -- REQUIRED: for native Neovim LSP integration
  --  lazy = false, -- REQUIRED: tell lazy.nvim to start this plugin at startup
  --  dependencies = {
  --    -- main one
  --    { "ms-jpq/coq_nvim", branch = "coq" },
  --
  --    -- 9000+ Snippets
  --    { "ms-jpq/coq.artifacts", branch = "artifacts" },
  --
  --    -- lua & third party sources -- See https://github.com/ms-jpq/coq.thirdparty
  --    -- Need to **configure separately**
  --    { 'ms-jpq/coq.thirdparty', branch = "3p" }
  --    -- - shell repl
  --    -- - nvim lua api
  --    -- - scientific calculator
  --    -- - comment banner
  --    -- - etc
  --  },
  --  init = function()
  --    vim.g.coq_settings = {
  --        auto_start = true, -- if you want to start COQ at startup
  --        -- Your COQ settings here
  --    }
  --  end,
  --  config = function()
  --    -- Your LSP settings here
  --  end,
  --},

  -- nvim-treesitter/nvim-treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    opts = {},
    config = function()
      vim.cmd('TSUpdate')
    end
  },

  -- telescope:
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
  },

  -- zig.vim
  {
    'ziglang/zig.vim',
    opts = {},
    -- Optional dependencies
    dependencies = {},
    config = function()
      -- Do nothing
    end,
  },

  -- oil.nvim
  {
    'stevearc/oil.nvim',
    opts = {},
    -- Optional dependencies
    dependencies = {"nvim-tree/nvim-web-devicons"},
    config = function ()
      require('oil').setup()
    end
  },

  -- leap.nvim
  --{
  --  'ggandor/leap.nvim',
  --  opts = {},
  --  config = function()
  --    require('leap').create_default_mappings()
  --  end
  --},

  -- vimwiki
  {
    'vimwiki/vimwiki',
    opts = {},
    config = function()
      -- Do nothing
    end
  },

  -- catppuccin.nvim
  {
    'catppuccin/nvim',
    lazy = false,
  },

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
  --,{
  --  'dense-analysis/ale',
  --  opts = {},
  --  config = function()
  --    -- Do nothing
  --  end
  --}

  -- nvim-dap
  --,{
  --  'mfussenegger/nvim-dap',
  --  opts = {},
  --  config = function()
  --    -- Do nothing
  --  end
  --}
  
} -- end plugins definition

-- Setup function
function m.setup()
  -- Utilize lazy.nvim plugin manager
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable", -- latest stable release
      lazypath,
    })
  end
  vim.opt.runtimepath:prepend(lazypath)
  
  
  -- Invoke lazy.nvim to setup desired plugins
  require('lazy').setup(m.plugins, {})
  
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
end

-- Init function
function lazyvim.init()
  m.setup()
end

-- Export module
return lazyvim
