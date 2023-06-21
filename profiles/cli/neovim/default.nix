{ pkgs, inputs, ... }: {
  home.packages = (with pkgs; [ lua-language-server ])
    ++ [ inputs.nixd.packages.${pkgs.system}.nixd ];
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = import ./plugins.nix pkgs;
    extraConfig = import ./customRc.nix;
  };
}
