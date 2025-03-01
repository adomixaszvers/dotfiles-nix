{
  programs.ghostty = {
    settings = {
      keybind = [
        "ctrl+shift+s=paste_from_selection"
        "ctrl+enter=new_split:auto"
      ];
      window_decoration = "server";
    };
  };
  stylix.targets.ghostty.enable = true;
}
