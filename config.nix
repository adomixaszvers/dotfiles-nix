{ lib, ... }:
{
  allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts" # used in onlyoffice
      "idea"
      "oraclejdk"
      "sqldeveloper"
      "steam"
      "steam-original" # steam without fhs
      "steam-run"
      "steam-unwrapped"
      "unrar"
    ];
  vim.ftNix = false;
  joypixels.acceptLicense = true;
  permittedInsecurePackages = [
    "oraclejdk-8u202"
    "gradle-7.6.6"
  ];
}
