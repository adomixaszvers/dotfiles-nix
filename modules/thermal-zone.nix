{ lib, ... }:
with lib;
{
  options.gui.thermal-zone = mkOption {
    type = types.ints.unsigned;
    default = 0;
    description = "Thermal zone number";
  };
}
