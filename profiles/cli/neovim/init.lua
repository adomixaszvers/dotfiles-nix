vim.g.mapleader = " "

-- Tab specific options
vim.o.tabstop = 8 -- A tab is 8 spaces
vim.o.expandtab = true -- Always uses spaces instead of tabs
vim.o.softtabstop = 4 -- Insert 4 spaces when tab is pressed
vim.o.shiftwidth = 4 -- An indent is 4 spaces
vim.o.shiftround = true -- Round indent to nearest shiftwidth multiple

vim.o.number = true
vim.o.relativenumber = true
vim.opt.path:append("**")
vim.o.list = true
vim.o.inccommand = "nosplit" -- Shows the effects of a command incrementally, as you type.
vim.o.background = "dark"
vim.o.cursorline = true
vim.o.scrolloff = 5 -- minimal lines visible above and below cursor

vim.o.showmode = false
vim.o.mouse = '';

vim.g.rainbow_active = 1

do
    local opts = { noremap=true, silent=true }
    vim.keymap.set('n', '<leader><leader>', '<cmd>Telescope find_files<cr>', opts)
    vim.keymap.set('n', '<leader>/', '<cmd>Telescope live_grep<cr>', opts)
    vim.keymap.set('n', "<leader>'", '<cmd>Telescope resume<cr>', opts)
    vim.keymap.set('n', "<leader>,", '<cmd>Telescope buffers<cr>', opts)
    vim.keymap.set('n', '<leader>f', '<cmd>Neoformat<cr>', opts)
    vim.keymap.set('n', '-', '-', opts)
    vim.keymap.set('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
    vim.keymap.set('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
    vim.keymap.set('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
    vim.keymap.set('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
end

vim.o.updatetime = 100
vim.o.grepprg = "rg --vimgrep --no-heading --smart-case"
vim.g.suda_smart_edit = 0
vim.g.ale_disable_lsp = 1
vim.g.ale_use_neovim_diagnostics_api = 1


if vim.fn.has("termguicolors") then
    vim.o.termguicolors = true
end

if vim.env.TERM ~= "linux" then
    vim.cmd('colorscheme nordic')
    vim.g.airline_powerline_fonts = 1
else
    vim.cmd('colorscheme solarized')
    vim.g.airline_symbols_ascii = 1
    vim.g.guicursor = ''
end

local nix_flakes_ag = vim.api.nvim_create_augroup('nix-flakes', { clear = true })
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
    pattern = 'flake.lock',
    group = nix_flakes_ag,
    callback = function()
        vim.bo.filetype = 'json'
    end
})

vim.o.timeout = true
vim.o.timeoutlen = 300
require('which-key').setup {}

require('telescope').load_extension('fzf')
