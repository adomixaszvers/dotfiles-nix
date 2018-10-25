{ pkgs, ... }:
{
  programs.neovim  = {
    enable = true;
    configure = {
      customRC = ''
          set number
          set relativenumber
          set path+=**
          set list
          set inccommand=nosplit
          colorscheme google

          let g:neomake_open_list = 2
          call neomake#configure#automake('nrwi', 500)

          let g:hardtime_default_on = 1
          let g:hardtime_showmsg = 1
          let g:hardtime_allow_different_key = 1
      '';
      vam.knownPlugins = pkgs.vimPlugins;
      vam.pluginDictionaries = [
        { name = "commentary"; }
        { name = "fugitive"; }
        { name = "lightline-vim"; }
        { name = "neomake"; }
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
