{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = import ./plugins.nix pkgs;
    extraLuaConfig = import ./customRc.nix;
    extraPackages = with pkgs; [ lua-language-server ];
  };
}
