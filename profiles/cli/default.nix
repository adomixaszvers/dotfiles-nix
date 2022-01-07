{ pkgs, inputs, myPkgs, ... }:

{
  imports = [
    ./direnv
    ./git.nix
    ./gpg-agent.nix
    ./flakeInputs.nix
    ./neovim
    ./kakoune
    ./registry.nix
    ./zsh
  ];
  home.packages = (with pkgs; [
    bat
    bfs
    binutils
    cachix
    dnsutils
    fd
    file
    gdb
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
    nix-linter
    p7zip
    patchelf
    ripgrep
    shellcheck
    statix
    tree
    unzip
    wol
    xdg-user-dirs
  ]) ++ (with myPkgs; [ hm-switch ]) ++ [
    (import inputs.comma {
      inherit pkgs;
      nix = pkgs.nix_2_4;
    })
  ];
  home.sessionVariables = {
    EDITOR = "nvim";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";
  };
  programs.bash = {
    enable = true;
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
  };
  programs.broot.enable = true;
  programs.exa = {
    enable = true;
    enableAliases = true;
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
