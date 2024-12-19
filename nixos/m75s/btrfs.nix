let
  commonBtrfsOptions = [
    "compress=zstd"
    "relatime"
    "discard=async"
    "commit=120"
  ];
in
{
  fileSystems = {
    "/".options = commonBtrfsOptions;
    "/home".options = commonBtrfsOptions;
    "/tmp".options = [
      "compress=zstd"
      "relatime"
      "nosuid"
      "nodev"
    ];
    "/nix".options = [
      "compress=zstd"
      "noatime"
      "discard=async"
      "commit=120"
    ];
  };
}
