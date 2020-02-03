{ pkgs, config, ... }: {
  imports = [ ../cli ../gui ../wm/common.nix ../wm/awesome ];
  home.packages = with pkgs;
    let unstable = channels.nixos-unstable;
    in [
      # (mine.steam.override { config.steam.primus = true; })
      borgbackup
      calibre
      compsize
      discord
      exercism
      firefox
      gtypist
      guile
      keepassxc
      klavaro
      mine.steam
      mine.vimgolf
      minecraft
      playerctl
      qbittorrent
      unstable.spotify
      tigervnc
      torbrowser
      unstable.jetbrains.idea-ultimate
      vim
    ];
  home.sessionVariables = { BROWSER = "firefox"; };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 60;
  };
}
