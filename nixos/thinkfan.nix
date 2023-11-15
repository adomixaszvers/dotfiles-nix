{ lib, ... }: {
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1
  '';
  services.thinkfan = {
    enable = lib.mkDefault true;
    levels = [ [ 0 0 61 ] [ "level auto" 60 32767 ] ];
  };
}
