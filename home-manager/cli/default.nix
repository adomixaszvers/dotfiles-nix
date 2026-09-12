{
  config,
  pkgs,
  myPkgs,
  inputs,
  lib,
  ...
}:

{
  imports = [
    (inputs.nix-wrapper-modules.lib.getInstallModule {
      name = "neovim";
      value = inputs.self.wrapperModules.neovim;
    })
    (inputs.nix-wrapper-modules.lib.getInstallModule {
      name = "neovim-nix";
      value = inputs.self.wrapperModules.neovim-nix;
    })
    ./direnv
    ./git.nix
    ./gnupg.nix
    ./nushell
    ./registry.nix
    ./repack-nix-tarball-cache.nix
    ./ssh-agent.nix
    ./zsh
  ];
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        # keep-sorted start
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
        nix-tree
        nixfmt
        p7zip
        patchelf
        ripgrep
        shellcheck
        statix
        tree
        unzip
        wol
        xdg-user-dirs
        # keep-sorted end
        ;
      inherit (myPkgs)
        # keep-sorted start
        he
        hm-repl
        # keep-sorted end
        ;
    };
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
      changeDirWidget.command = "bfs -type d";
      defaultCommand = "fd --type f";
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      fileWidget.command = "fd --type f";
      historyWidget.command = ""; # should be handled by atuin
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
      plugins = [
        {
          plugin = pkgs.tmuxPlugins.mode-indicator;
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
  wrappers.neovim.enable = lib.mkDefault true;
}
