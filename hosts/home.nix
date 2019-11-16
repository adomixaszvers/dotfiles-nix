{ pkgs, config, ... }: {
  imports = [ ../cli ../gui ../wm ];
  home.packages = with pkgs;
    let unstable = channels.nixos-unstable;
    in [
      borgbackup
      calibre
      discord
      exercism
      firefox
      gtypist
      guile
      klavaro
      minecraft
      (mine.steam.override { config.steam.primus = true; })
      mine.vimgolf
      qbittorrent
      torbrowser
      unstable.jetbrains.idea-ultimate
      vim
    ];
}
