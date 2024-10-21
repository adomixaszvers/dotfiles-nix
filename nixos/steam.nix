{ pkgs, ... }:
{
  programs = {
    steam = {
      enable = true;
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
              (sleep 1; pgrep gamescope| xargs renice -n -11 -p)&
              if [ -z "$WAYLAND_DISPLAY" ]; then
                exec gamescope "$@"
              else
                exec env LD_PRELOAD="" gamescope "$@"
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
