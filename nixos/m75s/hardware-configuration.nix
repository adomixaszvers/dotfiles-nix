# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "ehci_pci"
    "nvme"
    "usb_storage"
    "usbhid"
    "sd_mod"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/d6b8b1a9-dc2f-4bc8-a3ed-66013e2dcda6";
    fsType = "btrfs";
    options = [ "subvol=root/nixos/current" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/d6b8b1a9-dc2f-4bc8-a3ed-66013e2dcda6";
    fsType = "btrfs";
    options = [ "subvol=home/current" ];
  };

  fileSystems."/tmp" = {
    device = "/dev/disk/by-uuid/d6b8b1a9-dc2f-4bc8-a3ed-66013e2dcda6";
    fsType = "btrfs";
    options = [ "subvol=local/tmp/current" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/d6b8b1a9-dc2f-4bc8-a3ed-66013e2dcda6";
    fsType = "btrfs";
    options = [ "subvol=local/nix/current" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/9D0A-EF86";
    fsType = "vfat";
    options = [
      "umask=077"
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7c4d8cd3-b733-43c2-9621-fe5df2f0362a"; }
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp11s0f0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
