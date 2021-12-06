{ lib, ... }:
with lib; {
  options.programs.xmobar.thermal-zone = mkOption {
    type = types.ints.unsigned;
    default = 0;
    description = "Thermal zone number";
  };
}
