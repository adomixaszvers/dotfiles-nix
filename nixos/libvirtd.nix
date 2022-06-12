{ pkgs, ... }: {
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      runAsRoot = false;
      ovmf = {
        enable = true;
        package = pkgs.OVMFFull;
      };
      swtpm.enable = true;
    };
  };
}
