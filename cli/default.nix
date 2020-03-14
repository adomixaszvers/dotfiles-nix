{ config, lib, pkgs, ... }:

{
  imports = [ ./lorri.nix ./neovim ./kakoune ];
  home.packages = with pkgs; [
    bfs
    binutils
    cachix
    exa
    file
    git
    htop
    icdiff
    iotop
    jq
    lf
    lsof
    ncdu
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
  programs.z-lua = {
    enable = true;
    options = [ "once" ];
  };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    initExtra = ''
      lorri_rebuild () {
        for i in $(fd -t f \\.envrc ~ --no-ignore-vcs --hidden -x echo {//}); do
          if [ -f $i/shell.nix ]; then
            (
              echo "rebuilding $i"
              cd $i
              lorri watch --once
            )
          fi;
        done
      }
    '';
    plugins = [{
      name = "fz";
      src = pkgs.fetchFromGitHub {
        owner = "changyuheng";
        repo = "fz";
        rev = "ae0cd3ab0f0daebc80256f1236eda3a3283e4c1c";
        sha256 = "0x3w03gcqhyhfhjfxvbp5m1i96ihq9l3m52w4xnpbkchqfsyw737";
      };
    }];
    sessionVariables = { FZ_HISTORY_CD_CMD = "_zlua"; };
    shellAliases = {
      he =
        "(hcd && nvim $(FZF_DEFAULT_COMMAND='fd --type f --no-ignore-vcs' fzf))";
      hcd = "cd ~/.config/nixpkgs";
      ls = "exa";
      la = "exa -a --group-directories-first";
      ll = "exa -al --group-directories-first";
      lt = "exa -aT";
    };
  };
}
