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
  environment.systemPackages = [
    myPkgs.neovim
  ]
  ++ (with pkgs; [
    # keep-sorted start
    fd
    git
    nixfmt
    ripgrep
    # keep-sorted end
  ]);
  users.users.nixos.openssh.authorizedKeys.keyFiles = [ ../nixos/keys/yubikey.pub ];
}
