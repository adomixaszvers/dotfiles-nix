{ pkgs, lib, ... }:
{
  programs = {
    steam = {
      enable = true;
      extraCompatPackages = lib.mkDefault [ pkgs.proton-ge-bin ];
      package = pkgs.steam.override {
        extraPkgs =
          pkgs: with pkgs; [
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXScrnSaver
            libpng
            libvorbis
            stdenv.cc.cc.lib
            libkrb5
            keyutils
            mangohud
            (writeShellScriptBin "launch-gamescope" ''
              if [ -z "$WAYLAND_DISPLAY" ]; then
                exec nice -n -11 -- gamescope "$@"
              else
                exec env LD_PRELOAD="" nice -n -11 -- gamescope "$@"
              fi
            '')
          ];
      };
    };
    # use
    # `gamescope -f -- %command% & sleep 2 && renice -n -11 -p $(pgrep gamescope)`
    # as launch options
    gamescope = {
      enable = true;
      env = {
        XKB_LAYOUT = "us";
        XKB_DEFAULT_LAYOUT = "us";
      };
    };
  };
}
