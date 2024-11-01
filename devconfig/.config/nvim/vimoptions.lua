-- Module definition
local vimoptions = {}

-- Init function
function vimoptions.init()
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
end

-- Export module
return vimoptions
