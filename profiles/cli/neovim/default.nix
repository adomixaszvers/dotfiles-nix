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
