{ pkgs, lib, ... }:
let
  package = lib.throwIfNot (
    pkgs.openssh.version == "10.1p1"
  ) "The version of OpenSSH has been changed from 10.1p1" pkgs.openssh_10_2;
in
{
  programs.ssh = { inherit package; };
  services.openssh = {
    inherit package;
  };
}
