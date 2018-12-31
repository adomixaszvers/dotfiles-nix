{ pkgs, ... }:
{
  programs.neovim  = {
    enable = true;
    configure = {
      customRC = ''
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

          let g:LanguageClient_serverCommands = {
            \ 'haskell': ['${pkgs.hies}/bin/hie-wrapper'],
            \ 'python': ['${pkgs.pythonPackages.python-language-server}/bin/pyls'],
            \}
          nnoremap <F5> :call LanguageClient_contextMenu()<CR>
          map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
          map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
          map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
          map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
          map <Leader>lb :call LanguageClient#textDocument_references()<CR>
          map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
          map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

          let g:lightline = {}

          let g:lightline.component_expand = {
          \  'linter_checking': 'lightline#lsc#checking',
          \  'linter_warnings': 'lightline#lsc#warnings',
          \  'linter_errors': 'lightline#lsc#errors',
          \  'linter_ok': 'lightline#lsc#ok',
          \ }

          let g:lightline.component_type = {
          \     'linter_checking': 'left',
          \     'linter_warnings': 'warning',
          \     'linter_errors': 'error',
          \     'linter_ok': 'left',
          \ }

          let g:lightline.active = { 'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]] }

          noremap - -
      '';
      vam.knownPlugins = pkgs.vimPlugins // {
        lightline-lsc-nvim = pkgs.vimUtils.buildVimPlugin {
          name = "lightline-lsc-nvim";
          src = pkgs.fetchFromGitHub {
            owner = "palpatineli";
            repo = "lightline-lsc-nvim";
            rev = "b7be53d276ff0421b22dab9cbccd28e6a4fabeb8";
            sha256 = "0mrdfvdipx08c7z5f1a96a67cpbapz1rh6jl98ckzhmm2k6p439r";
          };
        };
      };
      vam.pluginDictionaries = [
        { name = "commentary"; }
        { name = "fugitive"; }
        { name = "fzf-vim"; }
        { name = "fzfWrapper"; }
        { name = "LanguageClient-neovim"; }
        { name = "lightline-vim"; }
        { name = "lightline-lsc-nvim"; }
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
      ];
    };
  };
}
