{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs;
    let unstable = pkgs.channels.nixos-unstable;
    in [
      corefonts
      google-fonts
      material-icons
      unstable.nerdfonts
      noto-fonts-emoji
    ];
}
