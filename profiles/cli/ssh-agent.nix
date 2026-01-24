{ pkgs, ... }:
{
  programs.zsh.shellAliases.sshyk = "ssh-add -s $(nix path-info nixpkgs\\#yubico-piv-tool.^out)/lib/libykcs11.so";
  services.ssh-agent = {
    enable = true;
    pkcs11Whitelist = [ "${pkgs.yubico-piv-tool}/lib/*" ];
  };
}
