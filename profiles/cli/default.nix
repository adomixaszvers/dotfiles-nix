{ pkgs, ... }:

{
  # imports = [ ./gpg-agent.nix ./lorri.nix ./neovim ./kakoune ];
  imports = [ ./gpg-agent.nix ./lorri.nix ./novim.nix ./kakoune ];
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
  home.sessionVariables = { EDITOR = "kak"; };
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
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    plugins = [{
      name = "fz";
      src = pkgs.nivSources.fz;
    }];
    sessionVariables = { FZ_HISTORY_CD_CMD = "_zlua"; };
    shellAliases = {
      he =
        "(hcd && FZF_DEFAULT_COMMAND='fd --type f --no-ignore-vcs' nvim -c ':Files')";
      hcd = "cd ~/.config/nixpkgs";
      ls = "exa";
      la = "exa -a --group-directories-first";
      ll = "exa -al --group-directories-first";
      lt = "exa -aT";
      setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
    };
    initExtra = ''
      find-shells () {
        cat $XDG_DATA_HOME/direnv/allow/* | sort | uniq | sed -e 's/\.envrc$/shell.nix/'
      }
    '';
  };
}
