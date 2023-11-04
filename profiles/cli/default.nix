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
      initExtra = ''
        hcd () {
          cd ~/.config/nixpkgs || exit 1
        }
        he () {
          (
            local FILES
            hcd && readarray -t FILES <<< "$(fzf --multi)" && [ -n "''${FILES[*]}" ] && "$EDITOR" "''${FILES[@]}"
            )
          }

      '';
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
