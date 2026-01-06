{
  networking.nat.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      runAsRoot = false;
      swtpm.enable = true;
    };
  };
}
