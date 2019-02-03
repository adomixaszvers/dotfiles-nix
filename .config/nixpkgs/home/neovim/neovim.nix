{ pkgs, config, ... }:
let
  lscConfig = import ./lscConfig.nix { inherit pkgs config; lib = pkgs.lib;};
  customPlugins = {
    lightline-bufferline = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-bufferline";
      src = pkgs.fetchFromGitHub {
        owner = "mengelbrecht";
        repo = "lightline-bufferline";
        rev = "467bf6894621f74845045ca0dd5b0841f5607491";
        sha256 = "1lf6bpl1zzl5hx9f8pw8rlzcrl1as6xh4nhw34pz670hp60yiryh";
      };
    };
  };
in
  {
    programs.neovim  = {
      enable = true;
      configure = {
        customRC = ''
        let mapleader = " "

        " Tab specific option
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
        colorscheme google

        set noshowmode

        let g:neomake_open_list = 2
        call neomake#configure#automake('nrwi', 500)

        let g:hardtime_default_on = 1
        let g:hardtime_showmsg = 1
        let g:hardtime_allow_different_key = 1

        let g:rainbow_active = 1
        map <leader>f :Files<CR>
        noremap - -

        let g:lightline = {
          \   'colorscheme': 'solarized',
          \   'active': {
          \     'left':[ [ 'mode', 'paste' ],
          \              [ 'gitbranch', 'readonly', 'filename', 'modified' ]
          \     ],
          \     'right': [ [ 'lineinfo' ],
          \            [ 'percent' ],
          \            [ 'fileformat', 'fileencoding', 'filetype' ] ]
          \   },
          \   'component': {
          \     'lineinfo': ' %3l:%-2v',
          \   },
          \   'component_function': {
          \     'gitbranch': 'fugitive#head',
          \   }
          \ }
        let g:lightline.tabline          = {'left': [['buffers']], 'right': [['close']]}
        let g:lightline.component_expand = {'buffers': 'lightline#bufferline#buffers'}
        let g:lightline.separator = {
        \   'left': '', 'right': ''
        \}
        let g:lightline.subseparator = {
        \   'left': '', 'right': ''
        \}
        let g:lightline.component_type   = {'buffers': 'tabsel'}
        set showtabline=2
        autocmd BufWritePost,TextChanged,TextChangedI * call lightline#update()

        set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
        '' + lscConfig.customRC;
        vam.knownPlugins = pkgs.vimPlugins // customPlugins // lscConfig.customPlugins;
        vam.pluginDictionaries = [
          { name = "commentary"; }
          { name = "fugitive"; }
          { name = "fzf-vim"; }
          { name = "fzfWrapper"; }
          { name = "lightline-bufferline"; }
          { name = "lightline-vim"; }
          { name = "neomake"; }
          { name = "rainbow"; }
          { name = "repeat"; }
          { name = "rust-vim"; }
          { name = "surround"; }
          { name = "syntastic"; }
          { name = "vim-addon-nix"; }
          { name = "vim-colorschemes"; }
          { name = "vim-easymotion"; }
          { name = "vim-hardtime"; }
          { name = "vim-nix"; }
          { name = "vinegar"; }
          { name = "vim-gitgutter"; }
        ] ++ lscConfig.pluginDictionaries;
      };
    };
  }
