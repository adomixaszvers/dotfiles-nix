{ inputs, ... }:
{
  imports = [
    (inputs.nix-wrapper-modules.lib.getInstallModule {
      name = "niri";
      value = inputs.self.wrapperModules.niri;
    })
    ./polybar-program.nix
    ./thermal-zone.nix
  ];
}
