{ config, lib, pkgs, ... }:

{
  imports = [ ./lorri.nix ./neovim ./kakoune ];
  home.packages = with pkgs; [
    bfs
    cachix
    file
    git
    htop
    icdiff
    iotop
    jq
    lsof
    ncdu
    nixfmt
    p7zip
    ranger
    ripgrep
    shellcheck
    tree
    unzip
    wol
    xdg-user-dirs
  ];
  home.sessionVariables = { EDITOR = "nvim"; };
  programs.bash = {
    enable = true;
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
  };
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    changeDirWidgetCommand = "bfs -type d";
    defaultCommand = "fd --type f";
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    fileWidgetCommand = "fd --type f";
  };
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    terminal = "screen-256color";
  };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      he =
        "(hcd && nvim $(FZF_DEFAULT_COMMAND='fd --type f --no-ignore-vcs' fzf))";
      hcd = "cd ~/.config/nixpkgs";
    };
  };
}
