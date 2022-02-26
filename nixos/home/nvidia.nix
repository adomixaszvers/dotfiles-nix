{ config, ... }: {
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.displayManager.sddm.enable = true;
  hardware.nvidia.package =
    config.boot.kernelPackages.nvidiaPackages.legacy_390;
  hardware.nvidia.prime.sync.enable = true;
  # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
  hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
}
