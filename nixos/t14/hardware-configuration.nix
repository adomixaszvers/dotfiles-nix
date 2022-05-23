# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "rpool/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E3F6-26D2";
    fsType = "vfat";
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
    device = "rpool/home";
    fsType = "zfs";
  };

  fileSystems."/home/adomas/.local/share/libvirt" = {
    device = "rpool/adomas/libvirt";
    fsType = "zfs";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/6f2b136d-bcfd-46aa-a01e-2a03f65f04e9"; }];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp2s0f0.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp5s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp3s0.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}