{ pkgs, ... }: {
  home.packages = with pkgs; [ sumneko-lua-language-server ];
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = import ./plugins.nix pkgs;
    extraConfig = import ./customRc.nix;
  };
}
