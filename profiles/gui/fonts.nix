{ pkgs, ... }:

{
  home.packages = with pkgs; [
    corefonts
    material-icons
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "FiraMono"
        "JetBrainsMono"
        "Noto"
      ];
    })
    noto-fonts-emoji
  ];
}
