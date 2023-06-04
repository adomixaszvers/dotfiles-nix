{ inputs, ... }: {
  services.xrdp = {
    enable = true;
    package = inputs.self.packages.x86_64-linux.custom-xrdp;
  };
  security.pam.services.xrdp-sesman = {
    enableGnomeKeyring = true;
    fprintAuth = false;
  };
  networking.firewall.interfaces.ztzlgoe57z.allowedTCPPorts = [ 3389 ];
}
