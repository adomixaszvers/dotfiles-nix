{ pkgs, config, ... }:
let
  customPlugins = {
    lightline-ale = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-ale";
      src = pkgs.fetchFromGitHub {
        owner = "maximbaz";
        repo = "lightline-ale";
        rev = "dd59077f9537b344f7ae80f713c1e4856ec1520c";
        sha256 = "1f9v6nsksy36s5i27nfx6vmyfyjk27p2w2g6x25cw56b0r3sgxmx";
      };
    };
    lightline-bufferline = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-bufferline";
      src = pkgs.fetchFromGitHub {
        owner = "mengelbrecht";
        repo = "lightline-bufferline";
        rev = "87431565ccfcc4c9ac892ab271eae77920c191e5";
        sha256 = "0dmcabdp8768fnb0mzzcidz6bx1aj93fgm4l4z768ils3xycbyw8";
      };
    };
    lightline-neomake = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-neomake";
      src = pkgs.fetchFromGitHub {
        owner = "sinetoami";
        repo = "lightline-neomake";
        rev = "08271edbdb8b6efb21123cd602471a806dff1913";
        sha256 = "0gr4kpci2w38xskh2y588amzpp5grnp0qyi7a06vcsq930l0yq41";
      };
    };
  };
in {
  programs.neovim = {
    enable = true;
    extraConfig = ''
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
      set inccommand=nosplit
      set background=dark
      set cursorline

      " let g:solarized_termcolors=256
      " let g:solarized_termtrans=1
      colorscheme dracula

      set noshowmode

      let g:hardtime_default_on = 0
      let g:hardtime_showmsg = 1
      let g:hardtime_allow_different_key = 1

      let g:rainbow_active = 1
      map <leader>f :Files<CR>
      map <leader>n <Plug>(ale_next_wrap)
      map <leader>p <Plug>(ale_previous_wrap)
      noremap - -
      set updatetime=100

      let g:lightline = {
        \   'colorscheme': 'wombat',
        \   'active': {
        \     'left':[ [ 'mode', 'paste' ],
        \              [ 'gitbranch', 'readonly', 'filename', 'modified' ]
        \     ],
        \     'right': [ [ 'lineinfo' ],
        \            [ 'percent' ],
        \            [ 'fileformat', 'fileencoding', 'filetype' ],
        \            [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
        \     ]
        \   },
        \   'component': {
        \     'lineinfo': ' %3l:%-2v',
        \   },
        \   'component_function': {
        \     'gitbranch': 'fugitive#head',
        \   }
        \ }
      let g:lightline.tabline          = {'left': [['buffers']], 'right': [['close']]}
      let g:lightline.component_expand = {
        \ 'buffers': 'lightline#bufferline#buffers',
        \ 'linter_checking': 'lightline#ale#checking',
        \ 'linter_warnings': 'lightline#ale#warnings',
        \ 'linter_errors': 'lightline#ale#errors',
        \ 'linter_ok': 'lightline#ale#ok',
        \ }
      if !empty($DISPLAY)
        let g:lightline.separator = {
        \   'left': '', 'right': ''
        \}
        let g:lightline.subseparator = {
        \   'left': '', 'right': ''
        \}
      else
        set guicursor=
      endif
      let g:lightline.component_type   = {
        \ 'buffers': 'tabsel',
        \ 'linter_checking': 'left',
        \ 'linter_warnings': 'warning',
        \ 'linter_errors': 'error',
        \ 'linter_ok': 'left',
        \}
      set showtabline=2
      autocmd BufWritePost,TextChanged,TextChangedI * call lightline#update()

      set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
    '';
    plugins = with pkgs.vimPlugins // customPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      fzfWrapper
      lightline-ale
      lightline-bufferline
      lightline-vim
      rainbow
      repeat
      surround
      vim-colorschemes
      vim-easymotion
      vim-gitgutter
      vim-hardtime
      vim-polyglot
      vim-unimpaired
      vinegar
    ];
  };
}
