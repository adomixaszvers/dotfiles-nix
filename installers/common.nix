{
  myPkgs,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ../nixos/flakes.nix
    ../nixos/nix-registry.nix
  ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  boot = {
    supportedFilesystems = [ "zfs" ];
  };
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      # keep-sorted start
      fd
      git
      nixfmt
      ripgrep
      # keep-sorted end
      ;
    inherit (myPkgs) neovim;
  };
  users.users.nixos.openssh.authorizedKeys.keyFiles = [ ../nixos/keys/yubikey.pub ];
}
