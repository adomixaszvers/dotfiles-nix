{ inputs, pkgs, ... }:
{
  imports = [
    inputs.noctalia-shell.homeModules.default
  ];
  programs = {
    noctalia-shell = {
      enable = true;
      package = pkgs.noctalia-shell;
      systemd.enable = true;
      settings = {
        appLauncher = {
          density = "condenced";
        };
        bar = {
          widgets = {
            left = [
              {
                id = "Launcher";
              }
              {
                id = "SystemMonitor";
              }
              {
                id = "ActiveWindow";
                maxWidth = 500;
              }
              {
                id = "MediaMini";
              }
            ];
            center = [
              {
                id = "Workspace";
              }
            ];
            right = [
              {
                id = "Tray";
              }
              {
                id = "NotificationHistory";
              }
              {
                id = "Battery";
              }
              {
                id = "Volume";
              }
              {
                id = "Brightness";
              }
              {
                id = "Clock";
              }
              {
                id = "ControlCenter";
              }
            ];
          };
        };
        location = {
          firstDayOfWeek = 1;
          name = "Vilnius";
        };
      };
    };
  };
  stylix.targets.noctalia-shell.enable = true;
}
