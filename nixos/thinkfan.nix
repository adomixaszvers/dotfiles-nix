{
  boot.extraModprobeConfig = ''
    options thinkpad_acpi experimental=1 fan_control=1
  '';
  services.thinkfan = {
    enable = true;
    levels = [
      [ 0 0 61 ]
      [ 1 60 66 ]
      [ 2 65 71 ]
      [ 3 70 76 ]
      [ 6 75 81 ]
      [ 7 80 32767 ]
    ];
  };
}
