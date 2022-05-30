{ pkgs, ... }: {
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = import ./plugins.nix pkgs;
    extraConfig = builtins.readFile ./init.vim;
  };
}
