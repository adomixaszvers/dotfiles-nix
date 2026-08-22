{ lib, ... }:
{
  allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      # keep-sorted start
      "corefonts" # used in onlyoffice
      "google-chrome"
      "idea"
      "jdk"
      "liquibase"
      "oraclejdk"
      "sqldeveloper"
      "steam"
      "steam-unwrapped"
      "unrar"
      # keep-sorted end
    ];
  permittedInsecurePackages = [
    "oraclejdk-8u202"
  ];
  vim.ftNix = false;
  joypixels.acceptLicense = true;
}
