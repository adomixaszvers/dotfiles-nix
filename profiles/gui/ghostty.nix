{
  programs.ghostty = {
    settings = {
      keybind = [
        "ctrl+shift+s=paste_from_selection"
        "ctrl+enter=new_split:auto"
        "ctrl+left_bracket=text:\\x1b"
      ];
    };
  };
  stylix.targets.ghostty.enable = true;
}
