-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  vim.cmd.ALEDisableBuffer()

  ---comment
  ---@param keys string
  ---@param func function
  ---@param desc string
  local nmap = function(keys, func, desc)
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  nmap('gD', vim.lsp.buf.declaration, 'LSP declaration')
  nmap('gd', vim.lsp.buf.definition, 'LSP definition')
  nmap('K', vim.lsp.buf.hover, 'LSP hover')
  nmap('gi', vim.lsp.buf.implementation, 'LSP implementation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'LSP signature_help')
  nmap('<space>wa', vim.lsp.buf.add_workspace_folder, 'LSP add_workspace_folder')
  nmap('<space>wr', vim.lsp.buf.remove_workspace_folder, 'LSP remove_workspace_folder')
  nmap('<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, 'LSP list workspace folders')
  nmap('<space>D', vim.lsp.buf.type_definition, 'LSP type_definition')
  nmap('<space>rn', vim.lsp.buf.rename, 'LSP rename')
  nmap('<space>ca', vim.lsp.buf.code_action, 'LSP code_action')
  nmap('<space>n', function()
    require 'telescope.builtin'.lsp_workspace_symbols { default_text = ':class: ' }
  end, 'LSP lsp_workspace_symbols')
end

vim.lsp.config('*', {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
})

vim.lsp.config('lua_ls', {
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file('', true),
        -- Ignore luassert
        checkThirdParty = false,
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
})

if vim.fn.executable('nixd') == 1 then
  vim.lsp.config('nixd', {
    settings = {
      nixd = {
        nixpkgs = {
          expr = 'import (builtins.getFlake ("git+file://" + toString ./.)).inputs.nixpkgs { }',
        },
        formatting = {
          command = { 'nixfmt' },
        },
        options = {
          nixos = {
            expr = '(builtins.getFlake ("git+file://" + toString ./.)).nixosConfigurations.adomo-t14.options',
          },
          home_manager = {
            expr = '(builtins.getFlake ("git+file://" + toString ./.)).homeConfigurations.t14.options',
          },
        },
      },
    },
  })
  vim.lsp.enable('nixd')
end

vim.lsp.config('nil_ls', {
  settings = {
    formatting = { command = { 'nixfmt' } },
  },
})

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = {
  'angularls',
  'hls',
  'lua_ls',
  'nil_ls',
  'pylsp',
  'rust_analyzer',
  'zls',
  'gopls',
  'taplo',
  'ts_ls',
}
for _, lsp in ipairs(servers) do
  vim.lsp.enable(lsp)
end
