{ pkgs, ... }:

{
  imports = [ ./gpg-agent.nix ./lorri.nix ./neovim ./kakoune ./zsh ];
  home.packages = with pkgs; [
    bat
    bfs
    binutils
    cachix
    exa
    fd
    file
    git
    htop
    icdiff
    iotop
    jq
    lf
    lsof
    ncdu
    niv
    nixfmt
    p7zip
    patchelf
    ranger
    ripgrep
    shellcheck
    tree
    unzip
    wol
    xdg-user-dirs
  ];
  home.sessionVariables = {
    EDITOR = "nvim";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";
  };
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
  programs.starship.enable = true;
  programs.tmux = {
    enable = true;
    extraConfig = ''
      set -g mouse on
    '';
    keyMode = "vi";
    terminal = "screen-256color";
  };
  programs.z-lua = {
    enable = true;
    options = [ "once" ];
  };
}
