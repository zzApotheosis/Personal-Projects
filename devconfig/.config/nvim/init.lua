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
vim.opt.rtp:prepend(lazypath)

-- Define desired plugins
local plugins = {
  -- oil.nvim
  {
    'stevearc/oil.nvim',
    opts = {},
    -- Optional dependencies
    dependencies = {"nvim-tree/nvim-web-devicons"},
    config = function ()
      require("oil").setup()
    end
  }

  -- leap.nvim
  --,{
  --  'ggandor/leap.nvim',
  --  opts = {},
  --  config = function()
  --    require('leap').create_default_mappings()
  --  end
  --}

  -- project.nvim
  ,{
   'ahmedkhalf/project.nvim',
    opts = {},
    config = function()
  	  require("project_nvim").setup()
  	end
  }

  -- nvim-tree.lua
  ,{
    'nvim-tree/nvim-tree.lua',
    opts = {},
    dependencies = {"nvim-tree/nvim-web-devicons"},
    config = function()
      require("nvim-tree").setup()
    end
  }

  -- ale
  ,{
    'dense-analysis/ale',
    opts = {},
    config = function()
      -- Do nothing
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
require("lazy").setup(plugins, {})

-- vim.lsp.start({
--   name = 'my-server-name',
--   cmd = {'clangd'},
--   root_dir = "/home/zzapotheosis/Development/Git/Personal-Projects/coding_practice/C"
-- })
