{ pkgs, ... }:

{
  home.packages = with pkgs; [
    corefonts
    google-fonts
    material-icons
    nerdfonts
    noto-fonts-emoji
  ];
  xdg.configFile."fontconfig/conf.d/99-disable-bad-font.conf".text = # xml
    ''
      <selectfont>
        <rejectfont>
          <glob>*/share/fonts/truetype/NotoColorEmoji-Regular.ttf</glob>
        </rejectfont>
      </selectfont>
    '';
}
