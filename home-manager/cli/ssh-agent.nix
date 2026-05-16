{ pkgs, ... }:
{
  home.packages =
    let
      sshyk = pkgs.writers.writeDashBin "sshyk" ''
        exec ssh-add -s ${pkgs.yubico-piv-tool}/lib/libykcs11.so
      '';
    in
    [ sshyk ];
  services.ssh-agent = {
    enable = true;
    pkcs11Whitelist = [ "${pkgs.yubico-piv-tool}/lib/*" ];
  };
}
