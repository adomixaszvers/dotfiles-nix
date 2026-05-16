{
  pkgs,
  config,
  lib,
  ...
}:
let
  padding =
    pkgs.runCommandLocal "padding-icon.sh"
      {
        script = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/jaor/xmobar/a819aed871c49d0924a811a407ccb3d8ad7bc7da/examples/padding-icon.sh";
          sha256 = "0qpdll5j6azg3z7cjlqz5dl0fcrr4vwszgv74n3l30xyiq3cpjfb";
        };
      }
      ''
        cp $script $out
        chmod u+x $out
        patchShebangs $out
      '';
  makeConfig =
    let
      thermalZone = toString config.gui.thermal-zone;
      inherit (config.lib.stylix)
        base00
        base05
        green
        red
        ;
    in
    hasTray: ''
      Config { font = "xft:NotoMono Nerd Font:fontformat=truetype:pixelsize=10:antialias=true"
            , additionalFonts = []
            , bgColor = "#${base00}"
            , fgColor = "#${base05}"
            , alpha = 255
            , position = Top
            , textOffset = -1
            , iconOffset = -1
            , lowerOnStart = True
            , pickBroadest = False
            , persistent = False
            , hideOnStart = False
            , iconRoot = "."
            , allDesktops = False
            , overrideRedirect = True
            , commands = [ Run Cpu ["-L","3","-H","50",
                                    "--normal","#${green}","--high","#${red}"] 10
                          , Run Memory ["-t","Mem: <usedratio>%"] 10
                          , Run Swap [] 10
                          , Run Date "%a %b %d %Y %H:%M" "date" 10
                          , Run UnsafeStdinReader
                          , Run Com "${padding}" ["panel"] "trayerpad" 10
                          , Run ThermalZone ${thermalZone} ["-t","<temp>°C"] 30
                          , Run Mpris2 "spotify" ["-t", "<artist> - <title>"] 10
                          ]
            , sepChar = "%"
            , alignSep = "}{"
            , template = "%UnsafeStdinReader% }\
                  \{ %mpris2% | %cpu% | %memory% * %swap% | %thermal${thermalZone}% | %date% ${lib.optionalString hasTray "| %trayerpad%"}"
            }
    '';
in
{
  home.packages = [ pkgs.xmobar ];
  services.pasystray.enable = true;
  services.trayer = {
    enable = true;
    settings = {
      edge = "top";
      SetDockType = true;
      SetPartialStrut = true;
      align = "right";
      expand = true;
      widthtype = "request";
      height = 17;
      alpha = 0;
      transparent = true;
      monitor = "primary";
      tint = "0xff${config.lib.stylix.base00}";
    };
  };
  xdg.configFile."xmobar/xmobarrc".text = makeConfig true;
  xdg.configFile."xmobar/xmobarrc_without_tray".text = makeConfig false;
}
