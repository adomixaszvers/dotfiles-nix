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
    ./nushell
  ];
  home = {
    packages = (with pkgs; [
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
    ]) ++ (with myPkgs; [ he hm-repl hm-switch ]);
    sessionVariables = {
      EDITOR = "nvim";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
    };
    shellAliases.hcd = "cd ~/.config/nixpkgs";
  };
  programs = {
    bash = {
      enable = true;
      historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
    };
    broot.enable = true;
    eza = {
      enable = true;
      enableAliases = true;
      package = pkgs.eza.overrideAttrs {
        patches = [
          (pkgs.fetchpatch {
            url =
              "https://github.com/eza-community/eza/commit/4b4fd1933c637afc5ac990803c8bc11972d10cb0.patch";
            sha256 = "0i7vl16lgz9fkv3bpdwd8krlls6ba03lrrjjwvkz6fdaa7nb2210";
          })
        ];
      };
    };
    fzf = {
      changeDirWidgetCommand = "bfs -type d";
      defaultCommand = "fd --type f";
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      fileWidgetCommand = "fd --type f";
    };
    lazygit.enable = true;
    tmux = {
      enable = true;
      keyMode = "vi";
      mouse = true;
      terminal = "screen-256color";
      plugins = with pkgs.tmuxPlugins; [{
        plugin = mode-indicator;
        extraConfig = ''
          set -g status-right '%Y-%m-%d %H:%M #{tmux_mode_indicator}'
        '';
      }];
    };
    z-lua = {
      enable = true;
      options = [ "once" ];
    };
  };
}
