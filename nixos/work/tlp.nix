{
  services.tlp = {
    enable = true;
    settings = {
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "low-power";
    };
  };
  services.thinkfan = {
    enable = true;
    levels = [
      [ 0 0 55 ]
      [ 1 48 60 ]
      [ 2 50 61 ]
      [ 3 52 63 ]
      [ 6 56 65 ]
      [
        7
        60
        32767
      ]
      # [ 7 60 85 ]
      # [ "level auto" 80 90 ]
      # [ "level full-speed" 85 32767 ]
    ];
  };
}
