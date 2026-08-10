{ lib, ... }:
{
  allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts" # used in onlyoffice
      "google-chrome"
      "jdk"
      "idea"
      "oraclejdk"
      "steam"
      "steam-unwrapped"
      "sqldeveloper"
      "unrar"
    ];
  permittedInsecurePackages = [
    "oraclejdk-8u202"
  ];
  vim.ftNix = false;
  joypixels.acceptLicense = true;
}
