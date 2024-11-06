# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ lib, ... }:

{

  boot.initrd.availableKernelModules = [
    "ehci_pci"
    "ahci"
    "xhci_pci"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "rpool/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };

  fileSystems."/var/lib/libvirt" = {
    device = "rpool/root/libvirt";
    fsType = "zfs";
  };

  fileSystems."/tmp" = {
    device = "rpool/local/tmp";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "hpool/home";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/fast" = {
    device = "rpool/adomas/fast";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/Maildir" = {
    device = "rpool/adomas/Maildir";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/GOG Games" = {
    device = "rpool/adomas/gog";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/.local/share/libvirt" = {
    device = "hpool/libvirtd";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/.local/share/Steam" = {
    device = "hpool/adomas/steam";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/615b64cf-a115-40d4-9beb-b1e12c186d37";
    fsType = "ext4";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/4de0a2d5-e10f-4975-a15a-4b0ec944cecf"; } ];

  nix.settings.max-jobs = lib.mkDefault 8;
  # powerManagement.cpuFreqGovernor = lib.mkDefault "schedutil";
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
