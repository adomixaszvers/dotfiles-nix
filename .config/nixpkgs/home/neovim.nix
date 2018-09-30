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

          let g:neomake_open_list = 2

          colorscheme google
          " packadd neomake
          call neomake#configure#automake('nrwi', 500)
          let g:hardtime_default_on = 1
      '';
      vam.knownPlugins = pkgs.vimPlugins;
      vam.pluginDictionaries = [
        { name = "fugitive"; }
        { name = "surround"; }
        { name = "vinegar"; }
        { name = "neomake"; }
        { name = "rust-vim"; }
        { name = "syntastic"; }
        { name = "vim-colorschemes"; }
        { name = "vim-addon-nix"; }
        { name = "vim-nix"; }
        { name = "vim-hardtime"; }
      ];
    };
  };
}
