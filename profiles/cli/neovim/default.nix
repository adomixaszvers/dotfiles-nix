{ unstable, ... }: {
  programs.neovim = {
    enable = true;
    package = unstable.neovim-unwrapped;
    plugins = import ./plugins.nix unstable;
    extraConfig = builtins.readFile ./init.vim;
  };
}
