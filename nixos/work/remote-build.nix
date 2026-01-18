{
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      system = "aarch64-linux";
      hostName = "rpi4";
      protocol = "ssh-ng";
    }
  ];
}
