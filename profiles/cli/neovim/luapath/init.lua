vim.g.mapleader = ' '

-- Tab specific options
vim.o.tabstop = 8 -- A tab is 8 spaces
vim.o.expandtab = true -- Always uses spaces instead of tabs
vim.o.softtabstop = 4 -- Insert 4 spaces when tab is pressed
vim.o.shiftwidth = 4 -- An indent is 4 spaces
vim.o.shiftround = true -- Round indent to nearest shiftwidth multiple

vim.o.number = true
vim.o.relativenumber = true
vim.opt.path:append('**')
vim.o.list = true
vim.o.inccommand = 'nosplit' -- Shows the effects of a command incrementally, as you type.
vim.o.background = 'dark'
vim.o.cursorline = true
vim.o.scrolloff = 5 -- minimal lines visible above and below cursor

vim.o.showmode = false
vim.o.mouse = ''

local conform = require('conform')
local fzf = require('fzf-lua')
fzf.setup({ 'telescope' })
fzf.register_ui_select()
do
  vim.keymap.set('n', '<leader><leader>', fzf.files, { desc = 'Find files' })
  vim.keymap.set('n', '<leader>/', fzf.live_grep, { desc = 'Live grep' })
  vim.keymap.set('n', "<leader>'", fzf.resume, { desc = 'Picker resume' })
  vim.keymap.set('n', '<leader>,', fzf.buffers, { desc = 'Buffers' })
  vim.keymap.set('n', '<leader><F1>', fzf.helptags, { desc = 'Help tags' })
  vim.keymap.set({ 'n', 'v' }, '<leader>f', function()
    conform.format({
      lsp_fallback = true,
      async = false,
      timeout_ms = 1000,
    })
  end, { desc = 'Format file' })
  vim.keymap.set('n', '-', '-', { noremap = true, silent = true })
  vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Diagnostic open_float' })
  vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Diagnostic setloclist' })
end

vim.o.updatetime = 100
vim.o.grepprg = 'rg --vimgrep --no-heading --smart-case'
vim.g.suda_smart_edit = 0
vim.g.ale_disable_lsp = 1
vim.g.ale_use_neovim_diagnostics_api = 1

if vim.env.TERM == 'linux' then
  vim.cmd('colorscheme solarized')
  vim.g.guicursor = ''
elseif vim.env.TERM == 'xterm' then
  vim.cmd('colorscheme default')
else
  vim.cmd([[
        colorscheme nordic
        packadd lualine.nvim
        packadd nvim-web-devicons
    ]])
  require('lualine').setup({
    options = {
      -- disabling section separators fixes
      -- the disappearing start screen issue
      section_separators = '',
    },
  })
end

vim.o.timeout = true
-- if the timeoutlen is too short I am too slow to input surround.vim bindings
-- so let the timeoutlen be 1000 (default)
-- vim.o.timeoutlen = 300

conform.setup({
  formatters_by_ft = {
    lua = { 'treefmt', 'stylua', stop_after_first = true },
    nix = { 'treefmt', 'nixfmt', stop_after_first = true },
  },
  formatters = {
    treefmt = function(bufnr)
      local dotfiles_root = vim.fs.joinpath(vim.env.XDG_CONFIG_HOME, 'nixpkgs')
      local bufname = vim.fn.expand('%:p')
      local fs = require('conform.fs')
      return { require_cwd = not fs.is_subpath(dotfiles_root, bufname) }
    end,
  },
})

vim.cmd.packadd('fidget.nvim')
require('fidget').setup({})
require('which-key').setup({})

require('gitsigns').setup({
  on_attach = function(bufnr)
    local gitsigns = require('gitsigns')

    ---comment
    ---@param mode string
    ---@param l string
    ---@param r function
    ---@param opts { desc: string, buffer?: number }
    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map('n', ']c', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']c', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end, { desc = 'Next hunk' })

    map('n', '[c', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[c', bang = true })
      else
        gitsigns.nav_hunk('prev')
      end
    end, { desc = 'Previous hunk' })

    -- Actions
    map('n', '<leader>hs', gitsigns.stage_hunk, { desc = 'Stage hunk' })
    map('n', '<leader>hr', gitsigns.reset_hunk, { desc = 'Reset hunk' })

    map('v', '<leader>hs', function()
      gitsigns.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end, { desc = 'Stage hunk' })

    map('v', '<leader>hr', function()
      gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end, { desc = 'Reset hunk' })

    map('n', '<leader>hS', gitsigns.stage_buffer, { desc = 'Stage buffer' })
    map('n', '<leader>hR', gitsigns.reset_buffer, { desc = 'Reset buffer' })
    map('n', '<leader>hp', gitsigns.preview_hunk, { desc = 'Preview hunk' })
    map('n', '<leader>hi', gitsigns.preview_hunk_inline, { desc = 'Preview hunk inline' })

    map('n', '<leader>hb', function()
      gitsigns.blame_line({ full = true })
    end, { desc = 'Blame line' })

    map('n', '<leader>hd', gitsigns.diffthis, { desc = 'gitsigns.diffthis' })

    map('n', '<leader>hD', function()
      gitsigns.diffthis('~')
    end, { desc = "gitsigns.diffthis('~')" })

    map('n', '<leader>hQ', function()
      gitsigns.setqflist('all')
    end, { desc = "gitsigns.setqflist('all')" })
    map('n', '<leader>hq', gitsigns.setqflist, { desc = 'gitsigns.setqflist' })

    -- Toggles
    map('n', '<leader>tb', gitsigns.toggle_current_line_blame, { desc = 'Toggle line blame' })
    map('n', '<leader>tw', gitsigns.toggle_word_diff, { desc = 'Toggle word diff' })

    -- Text object
    map({ 'o', 'x' }, 'ih', gitsigns.select_hunk, { desc = 'Select hunk' })
  end,
})

vim.api.nvim_create_autocmd('BufWritePre', {
  pattern = '*.nix',
  callback = function(args)
    conform.format({ bufnr = args.buf })
  end,
})

require('myConfig.nvim-treesitter')
local nixCats = require('nixCats')
if nixCats('lsp') then
  require('myConfig.lspconfig')
end
if nixCats('blink-cmp') then
  require('myConfig.blink-cmp')
end
if nixCats('cmp') then
  require('myConfig.cmp')
end
