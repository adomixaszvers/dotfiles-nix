{ pkgs, config, ... }:

{
  home.packages = let
    padding = pkgs.writeShellScriptBin "padding-icon.sh"
      (builtins.readFile ./padding-icon.sh);
  in [ pkgs.xmobar padding ];
  xdg.configFile."xmobar/xmobarrc".text = with config.colors; ''
    Config { font = "xft:NotoMono Nerd Font:fontformat=truetype:pixelsize=10:antialias=true"
          , additionalFonts = []
          , bgColor = "${background}"
          , fgColor = "${foreground}"
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
                                  "--normal","${green}","--high","${red}"] 10
                        , Run Memory ["-t","Mem: <usedratio>%"] 10
                        , Run Swap [] 10
                        , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                        , Run UnsafeStdinReader
                        , Run Com "padding-icon.sh" ["panel"] "trayerpad" 10
                        , Run ThermalZone 2 ["-t","<temp>°C"] 30
                        ]
          , sepChar = "%"
          , alignSep = "}{"
          , template = "%UnsafeStdinReader% }\
                \{ %cpu% | %memory% * %swap% | %thermal2% | %date% | %trayerpad%"
          }
  '';
}
