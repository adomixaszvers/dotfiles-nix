{ pkgs, ... }: {
  programs = {
    steam = {
      enable = true;
      package = pkgs.steam.override {
        extraPkgs = pkgs:
          with pkgs; [
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
          ];
      };
    };
    # use
    # `gamescope -f -- %command% & sleep 2 && renice -n -11 -p $(pgrep gamescope)`
    # as launch options
    gamescope = {
      enable = true;
      env.XKB_LAYOUT = "us";
      args = [ "--adaptive-sync" "-W 1920 -H 1080" ];
    };
  };
}
