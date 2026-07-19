{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # keep-sorted start
    corefonts
    material-icons
    nerd-fonts.fira-code
    nerd-fonts.fira-mono
    nerd-fonts.jetbrains-mono
    nerd-fonts.noto
    nerd-fonts.symbols-only
    noto-fonts-color-emoji
    # keep-sorted end
  ];
}
