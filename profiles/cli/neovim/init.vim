let mapleader = " "

" Tab specific options
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set shiftround                  "Round indent to nearest shiftwidth multiple

set number
set relativenumber
set path+=**
set list
set inccommand=nosplit " Shows the effects of a command incrementally, as you type.
set background=dark
set cursorline
set scrolloff=5 " minimal lines visible above and below cursor

" let g:solarized_termcolors=256
" let g:solarized_termtrans=1

set noshowmode

let g:rainbow_active = 1
nnoremap <leader><leader> <cmd>Telescope find_files<cr>
nnoremap <leader>n <Plug>(ale_next_wrap)
nnoremap <leader>p <Plug>(ale_previous_wrap)
nnoremap <leader>l :Neoformat<CR>
noremap - -
set updatetime=100

set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

if (has("termguicolors"))
  set termguicolors
endif

if $TERM != "linux"
    colorscheme nord
    let g:airline_powerline_fonts = 1
else
    colorscheme solarized
    let g:airline_symbols_ascii = 1
    set guicursor=
endif

augroup nix-flakes
    autocmd!
    autocmd BufNewFile,BufRead flake.lock set filetype=json
augroup END

let g:suda_smart_edit = 1

lua <<EOF
    require'nvim-treesitter.configs'.setup {
        highlight = {
            enable = true,              -- false will disable the whole extension
        },
        incremental_selection = {
            enable = true,
            keymaps = {
            init_selection = "<leader>gn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
            },
        },
    }
EOF
