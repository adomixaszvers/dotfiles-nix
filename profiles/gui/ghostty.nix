{
  programs.ghostty = {
    enable = true;
    settings = {
      keybind = [
        "ctrl+shift+s=paste_from_selection"
        "ctrl+enter=new_split:auto"
        "ctrl+left_bracket=text:\\x1b"
      ];
      shell-integration = "zsh";
      shell-integration-features = true;
    };
  };
  stylix.targets.ghostty.enable = true;
}
