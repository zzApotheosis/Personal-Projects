-- Module definition
local lsp = {}

-- Public-facing function to initialize LSP setup
function lsp.setup_lsp()
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
end

-- Public-facing function to stop all LSP clients
function lsp.stop_all_lsp()
  vim.lsp.stop_client(vim.lsp.get_active_clients())
end

-- Init function
function lsp.init()
  lsp.setup_lsp()
end

-- Export module
return lsp
