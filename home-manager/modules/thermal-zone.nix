{ lib, ... }:
{
  options.gui = {
    hasBattery = lib.mkOption {
      default = false;
      example = true;
      description = "Does it have a battery?";
      type = lib.types.bool;
    };
    thermal-zone = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 0;
      description = "Thermal zone number";
    };
  };
}
