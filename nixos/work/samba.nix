let
  mkMount = rw: {
    device = "//disk.insoft.dev/storage";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1000,gid=100";

    in [
      "${automount_opts},${
        if rw then "rw" else "ro"
      },credentials=/root/smb-secrets"
    ];
    noCheck = true;
  };
in {
  fileSystems."/kiti/insoft" = mkMount false;
  fileSystems."/kiti/insoft_rw" = mkMount true;
}
