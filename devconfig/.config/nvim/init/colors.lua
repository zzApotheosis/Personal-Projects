-- Module definition
local colors = {}
local m = {}

-- Setup function
function m.setup()
  vim.cmd('colorscheme catppuccin')
  m.bg_transparent()
end

-- Public-facing function to set transparent background
function m.bg_transparent()
  -- Set transparent background no matter what
  vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
end

-- Init function
function colors.init()
  m.setup()
end

-- Export module
return colors
