{ lib, ... }:
{
  allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts" # used in onlyoffice
      "idea"
      "replace"
      "steam"
      "steam-unwrapped"
      "unrar"
    ];
  vim.ftNix = false;
  joypixels.acceptLicense = true;
}
