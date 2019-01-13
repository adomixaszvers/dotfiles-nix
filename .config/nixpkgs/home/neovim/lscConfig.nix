{ pkgs, config, ...}:
(if config.lib.lsc.enable or false then
{
  customRC = ''
  let g:LanguageClient_serverCommands = {
  \   'haskell': ['${pkgs.hies}/bin/hie-wrapper'],
  \   'python': ['${pkgs.pythonPackages.python-language-server}/bin/pyls'],
  \}
  nnoremap <F5> :call LanguageClient_contextMenu()<CR>
  map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
  map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
  map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
  map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
  map <Leader>lb :call LanguageClient#textDocument_references()<CR>
  map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
  map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

  call extend(g:lightline.component_expand, {
  \  'linter_checking': 'lightline#lsc#checking',
  \  'linter_warnings': 'lightline#lsc#warnings',
  \  'linter_errors': 'lightline#lsc#errors',
  \  'linter_ok': 'lightline#lsc#ok',
  \ })

  call extend(g:lightline.component_type, {
  \     'linter_checking': 'left',
  \     'linter_warnings': 'warning',
  \     'linter_errors': 'error',
  \     'linter_ok': 'left',
  \ })

  let g:lightline.active = { 'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]] }

  '';
  customPlugins = {
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
  pluginDictionaries = [
    { name = "LanguageClient-neovim"; }
    { name = "lightline-lsc-nvim"; }
  ];
}
else {
  customRC = "";
  customPlugins = {};
  pluginDictionaries = [];
})
