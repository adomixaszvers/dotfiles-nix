{ pkgs, ... }: {
  home.packages = with pkgs; [ mine.rofi-powermenu ];
  services.pasystray.enable = true;
  xsession.windowManager.awesome = { enable = true; };
  xdg.configFile."awesome/rc.lua".source = ./rc.lua;
  xdg.configFile."awesome/scratch.lua".source = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/notnew/awesome-scratch/master/scratch.lua";
    sha256 = "0jskdpfkk4n23skgsa144pnljrgv5rb0gd5jmqlgpzg8729717sd";
  };
  xdg.configFile."awesome/sharedtags" = {
    source = pkgs.fetchFromGitHub {
      owner = "Drauthius";
      repo = "awesome-sharedtags";
      rev = "32d878d0d12bcfd900f2c6a7516a1370e6ebb7d6";
      sha256 = "0js6v2jmkczi3g8j7vbk9hsq5wfz96jnhi7ka5c1rqw22lmjmlrk";
    };
  };
}
