{
  pkgs,
  inputs,
  ...
}:
{
  imports = [ inputs.stylix.homeModules.stylix ];
  stylix = {
    autoEnable = false;
    enableReleaseChecks = false;
    image = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/wallpapers/nix-wallpaper-nineish-catppuccin-latte.png";
      sha256 = "09p6dyz5csx6gajbmdnnwk8fss4rzj87i07dz60lfk9m4i1anf7q";
    };

    polarity = "light";
    base16Scheme = "${inputs.stylix.inputs.tinted-schemes.outPath}/base24/catppuccin-latte.yaml";

    cursor = {
      name = "Banana";
      package = pkgs.banana-cursor;
      size = 24;
    };
    icons = {
      dark = "Papirus-Dark";
      light = "Papirus-Light";
      package = pkgs.papirus-icon-theme;

    };
    fonts = {
      monospace = {
        package = pkgs.nerd-fonts.jetbrains-mono;
        name = "JetBrainsMono Nerd Font Mono";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 10;
        terminal = 10;
      };
    };
  };
}
