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
          packadd neomake
          call neomake#configure#automake('nrwi', 500)
          let g:hardtime_default_on = 0
      '';
      packages.myNeoVim = with pkgs.vimPlugins; {
        start = [
          tlib
          fugitive
          surround
          vim-addon-actions
          vim-addon-completion
          vim-addon-errorformats
          vim-addon-goto-thing-at-cursor
          vim-addon-mw-utils
          vim-addon-nix
          vim-colorschemes
          vim-hardtime
          vim-hdevtools
          vim-nix
          vinegar
        ];
        opt = [ neomake ];
      };
    };
  };
}
