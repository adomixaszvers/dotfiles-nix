{ pkgs, ... }:
{
  gtk = {
    enable = true;
    iconTheme = {
      name = "Numix";
      package = pkgs.numix-icon-theme;
    };
    theme = {
      name = "Numix";
      package = pkgs.numix-gtk-theme;
    };
  };
  home.keyboard.layout = "lt,us";
  home.packages = import ./packages.nix {inherit pkgs;};
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.03.tar.gz;
  programs.neovim = import ./neovim.nix { inherit pkgs; };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    theme = "Monokai";
  };
  xsession.enable = true;
  xsession.windowManager.i3 = import ./i3.nix { inherit pkgs; };
}
