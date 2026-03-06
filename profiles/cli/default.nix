{
  config,
  pkgs,
  myPkgs,
  ...
}:

{
  imports = [
    ../../nixos/nix-package.nix
    ./direnv
    ./git.nix
    ./gnupg.nix
    ./neovim
    ./registry.nix
    ./ssh-agent.nix
    ./zsh
  ];
  nixpkgs.overlays = [
    (final: prev: {
      less =
        let
          version = "692";
          upstreamPackage = prev.less;
          updatedPackage = upstreamPackage.overrideAttrs {
            inherit version;
            src = final.fetchzip {
              url = "https://www.greenwoodsoftware.com/less/less-${version}.tar.gz";
              hash = "sha256-Imc5m0jh85vfsNhA9iqvfBb2MSQul7PYqm1Ppe75UGA=";
            };
          };
          isUpdatedVersion = final.lib.versionAtLeast upstreamPackage.version version;
        in
        final.lib.warnIf isUpdatedVersion "less was updated in nixpkgs" updatedPackage;
    })
  ];
  home = {
    packages =
      (with pkgs; [
        bat
        bfs
        binutils
        cachix
        comma
        deadnix
        dnsutils
        fd
        file
        github-cli
        htop
        icdiff
        jq
        lf
        lsof
        ncdu
        nixfmt
        nix-tree
        p7zip
        patchelf
        ripgrep
        shellcheck
        statix
        tree
        unzip
        wol
        xdg-user-dirs
      ])
      ++ (with myPkgs; [
        he
        hm-repl
      ]);
    sessionVariables = {
      EDITOR = "nvim";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
    };
    shellAliases.hcd = "cd ~/.config/nixpkgs";
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "-d --delete-older-than 14d";
  };
  programs = {
    bash = {
      enable = true;
      historyControl = [
        "erasedups"
        "ignoredups"
        "ignorespace"
      ];
    };
    broot.enable = true;
    eza.enable = true;
    fzf = {
      changeDirWidgetCommand = "bfs -type d";
      defaultCommand = "fd --type f";
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      fileWidgetCommand = "fd --type f";
    };
    git.enable = true;
    lazygit.enable = true;
    less.enable = true;
    nh = {
      enable = true;
      flake = "git+file://${config.xdg.configHome}/nixpkgs";
    };
    tmux = {
      enable = true;
      keyMode = "vi";
      mouse = true;
      terminal = "screen-256color";
      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = mode-indicator;
          extraConfig = ''
            set -g status-right '%Y-%m-%d %H:%M #{tmux_mode_indicator}'
          '';
        }
      ];
    };
    z-lua = {
      enable = true;
      options = [ "once" ];
    };
  };
}
