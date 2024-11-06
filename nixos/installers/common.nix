{
  myPkgs,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ../flakes.nix
    ../nix-registry.nix
  ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  boot = {
    supportedFilesystems = [ "zfs" ];
  };
  environment.systemPackages =
    [ myPkgs.neovim ]
    ++ (with pkgs; [
      fd
      git
      ripgrep
      nixfmt-rfc-style
    ]);
  users.users.nixos.openssh.authorizedKeys.keyFiles = [ ../keys/yubikey.pub ];
}
