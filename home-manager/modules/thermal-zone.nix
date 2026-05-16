{ lib, ... }:
with lib;
{
  options.gui = {
    hasBattery = mkOption {
      default = false;
      example = true;
      description = "Does it have a battery?";
      type = lib.types.bool;
    };
    thermal-zone = mkOption {
      type = types.ints.unsigned;
      default = 0;
      description = "Thermal zone number";
    };
  };
}
