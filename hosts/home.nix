{ pkgs, config, ... }: {
  imports = [ ../cli ../gui ];
  home.packages = with pkgs;
    let unstable = channels.nixos-unstable;
    in [
      # (mine.steam.override { config.steam.primus = true; })
      borgbackup
      calibre
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
      qbittorrent
      tigervnc
      torbrowser
      unstable.jetbrains.idea-ultimate
      vim
    ];
}
