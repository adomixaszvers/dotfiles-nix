{
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [ inputs.stylix.homeManagerModules.stylix ];
  stylix = {
    autoEnable = false;
    image = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/wallpapers/nix-wallpaper-simple-light-gray.png";
      sha256 = "0i6d0xv1nzrv7na9hjrgzl3jrwn81vnprnq2pxyznlxbjcgkjnk2";
    };

    polarity = "dark";
    base16Scheme = lib.mkDefault "${pkgs.base16-schemes}/share/themes/nord.yaml";

    cursor = {
      name = "Banana";
      package = pkgs.banana-cursor;
      size = 24;
    };
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" ]; };
        name = "JetBrainsMono Nerd Font Mono";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 10;
        terminal = 10;
      };
    };
  };
}
