{ pkgs, ... }: {
  imports =
    [ ./work-common.nix ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/nord.nix;
  programs.autorandr = {
    enable = true;
    profiles = {
      work = {
        fingerprint = {
          DP-1 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
          DP-2 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
        };
        config = {
          HDMI1.enable = false;
          HDMI2.enable = false;
          VGA1.enable = false;
          VIRTUAL1.enable = false;
          DP-1 = {
            enable = true;
            position = "0x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          DP-2 = {
            enable = true;
            position = "1920x0";
            primary = true;
            rate = "59.95";
            mode = "1920x1200";
          };
        };
      };
    };
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
  };
  xsession.windowManager.bspwm = {
    monitors = {
      "DP-1" = [ "1" "2" "3" "4" "5" ];
      "DP-2" = [ "6" "7" "8" "9" "10" ];
    };
  };
  xsession.initExtra = ''
    autorandr --change
  '';
  wayland.windowManager.sway.config = {
    startup = [{
      command =
        "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${pkgs.swaylock-effects}/bin/swaylock --clock --screenshots --effect-blur 7x5 --effect-vignette 0.5:0.5'";
    }];
    output = {
      DP-1 = { pos = "0 0"; };
      DP-2 = { pos = "1920 0"; };
    };
    input = {
      "6127:24729:Lenovo_Lenovo_Traditional_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}
