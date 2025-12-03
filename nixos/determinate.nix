{ inputs, ... }:
{
  imports = [
    inputs.determinate.nixosModules.default
  ];
  nix.settings.eval-cores = 4;
}
