{ pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      # keep-sorted start
      corefonts
      material-icons
      noto-fonts-color-emoji
      # keep-sorted end
      ;
    inherit (pkgs.nerd-fonts)
      # keep-sorted start
      fira-code
      fira-mono
      jetbrains-mono
      noto
      symbols-only
      # keep-sorted end
      ;
  };
}
