{ pkgs, ... }: {
  imports = [ ../picom.nix ../dunst.nix ../polybar.nix ];
  home.packages = with pkgs; [ dmenu xorg.xmodmap ];
  xsession.windowManager.command =
    let package = pkgs.callPackage ./my-penrose-config { };
    in "${package}/bin/my_penrose_config";
}
