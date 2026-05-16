{
  programs.ghostty = {
    enable = true;
    settings = {
      keybind = [
        "ctrl+enter=toggle_split_zoom"
        "ctrl+shift+s=paste_from_selection"
        "ctrl+shift+enter=new_split:auto"
        "ctrl+left_bracket=text:\\x1b"
      ];
      shell-integration-features = true;
    };
  };
  stylix.targets.ghostty.enable = true;
}
