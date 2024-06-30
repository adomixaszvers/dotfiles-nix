{ config, myPkgs, pkgs, ... }: {
  imports = [ ../flakes.nix ../nix-registry.nix ];
  boot = {
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    supportedFilesystems = [ "zfs" ];
  };
  environment.systemPackages = [ myPkgs.neovim ]
    ++ (with pkgs; [ fd git ripgrep nixfmt ]);
  users.users.nixos.openssh.authorizedKeys.keyFiles = [ ../keys/yubikey.pub ];
}
