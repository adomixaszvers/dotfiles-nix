{ pkgs, lib, ... }: {
  programs.neovim = {
    enable = true;
    extraConfig = lib.readFile ./vimrc;
    plugins = with pkgs.vimPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      fzfWrapper
      neoformat
      rainbow
      repeat
      {
        plugin = suda-vim;
        config = "let g:suda_smart_edit = 1";
      }
      surround
      vim-airline
      vim-airline-themes
      vim-colorschemes
      vim-gitgutter
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
    ];
  };
}
