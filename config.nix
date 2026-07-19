{ lib, ... }:
{
  allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts" # used in onlyoffice
      "google-chrome"
      "idea"
      "steam"
      "steam-unwrapped"
      "unrar"
    ];
  vim.ftNix = false;
  joypixels.acceptLicense = true;
}
