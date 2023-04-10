{ lib, ... }: {
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1
  '';
  services.thinkfan = {
    enable = lib.mkDefault true;
    levels = [
      [ 0 0 56 ]
      [ 1 55 66 ]
      [ 2 65 71 ]
      [ 3 70 76 ]
      [ 4 75 81 ]
      [ 5 80 86 ]
      [ 7 85 32767 ]
    ];
  };
}
