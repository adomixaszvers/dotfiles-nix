{ pkgs, config, lib, ... }: {
  programs.neovim = {
    enable = true;
    extraConfig = lib.readFile ./vimrc;
    plugins = with pkgs.vimPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      fzfWrapper
      rainbow
      repeat
      surround
      vim-airline
      vim-airline-themes
      vim-colorschemes
      vim-easymotion
      vim-gitgutter
      vim-hardtime
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
    ];
  };
}
