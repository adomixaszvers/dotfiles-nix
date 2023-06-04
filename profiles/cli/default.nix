{ pkgs, myPkgs, ... }:

{
  imports = [
    ./direnv
    ./git.nix
    ./gnupg.nix
    ./flakeInputs.nix
    ./neovim
    ./nix-index.nix
    ./kakoune
    ./registry.nix
    ./zsh
  ];
  home.packages = (with pkgs; [
    bat
    bfs
    binutils
    cachix
    comma
    dnsutils
    fd
    file
    git
    github-cli
    htop
    icdiff
    iotop
    jq
    lf
    lsof
    ncdu
    niv
    nixfmt
    nix-index
    p7zip
    patchelf
    ripgrep
    shellcheck
    statix
    tree
    unzip
    wol
    xdg-user-dirs
  ]) ++ (with myPkgs; [ hm-repl hm-switch ]);
  home.sessionVariables = {
    EDITOR = "nvim";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";
  };
  programs = {
    bash = {
      enable = true;
      historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
    };
    broot.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
    };
    fzf = {
      changeDirWidgetCommand = "bfs -type d";
      defaultCommand = "fd --type f";
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      fileWidgetCommand = "fd --type f";
    };
    tmux = {
      enable = true;
      extraConfig = ''
        set -g mouse on
      '';
      keyMode = "vi";
      terminal = "screen-256color";
    };
    z-lua = {
      enable = true;
      options = [ "once" ];
    };
  };
}
