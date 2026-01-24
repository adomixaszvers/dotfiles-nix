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
      url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/wallpapers/nix-wallpaper-nineish-catppuccin-frappe.png";
      sha256 = "03lrj64zig62ibhcss5dshy27kvw363gzygm4rgk7ihbdjj2sw7w";
    };

    polarity = "dark";
    base16Scheme = "${inputs.stylix.inputs.tinted-schemes.outPath}/base24/catppuccin-mocha.yaml";

    cursor = {
      name = "Banana";
      package = pkgs.banana-cursor;
      size = 24;
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
