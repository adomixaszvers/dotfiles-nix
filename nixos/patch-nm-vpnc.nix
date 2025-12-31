{ inputs, pkgs, ... }:
{
  networking.networkmanager.plugins = [
    inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.networkmanager-vpnc
  ];
}
