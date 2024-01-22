{ lib, ... }: {
  services.thinkfan = {
    enable = lib.mkDefault true;
    levels = [ [ 0 0 61 ] [ "level auto" 60 32767 ] ];
  };
}
