-- Module definition
local colors = {}

-- Public-facing function to set transparent background
function colors.bg_transparent()
  -- Set transparent background no matter what
  --vim.cmd('colorscheme catppuccin')
  vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
end

-- Init function
function colors.init()
  colors.bg_transparent()
end

-- Export module
return colors
