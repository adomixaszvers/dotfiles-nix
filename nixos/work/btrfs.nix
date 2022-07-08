let
  commonBtrfsOptions =
    [ "compress=zstd" "relatime" "discard=async" "commit=120" ];
in {

  fileSystems = {
    "/".options = commonBtrfsOptions;

    "/home".options = commonBtrfsOptions;

    "/var/lib/docker".options = commonBtrfsOptions;

    "/tmp".options = [ "compress=zstd" "relatime" "nosuid" "nodev" ];

    "/nix".options = [ "noatime" "discard=async" "commit=120" ];

    "/home/adomas/.local/share/libvirt".options = commonBtrfsOptions;

    "/var/lib/libvirt".options = commonBtrfsOptions;
  };

  boot.supportedFilesystems = [ "btrfs" ];

  services.btrfs.autoScrub.enable = true;
  services.beesd.filesystems = {
    rpool = {
      spec = "LABEL=rpool";
      hashTableSizeMB = 2048;
      verbosity = "crit";
      extraOptions = [ "--loadavg-target" "5.0" ];
    };
  };

}
