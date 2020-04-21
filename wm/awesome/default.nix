{ pkgs, ... }: {
  imports = [ ../picom.nix ];
  services.pasystray.enable = true;
  xsession.windowManager.awesome = { enable = true; };
  xdg.configFile = let
    copycats = pkgs.fetchFromGitHub {
      owner = "lcpz";
      repo = "awesome-copycats";
      rev = "06f05ac74098464d8987375b0784500a2317ac11";
      sha256 = "1ijf1861bl1ildk33h1jkbfli96grfk46gvranj1hz8p68svxnqd";
      fetchSubmodules = true;
    };
  in {
    "awesome/lain" = { source = "${copycats}/lain"; };
    "awesome/freedesktop" = { source = "${copycats}/freedesktop"; };
    "awesome/themes" = { source = "${copycats}/themes"; };
    "awesome/rc.lua" = {
      source = ./rc.lua;
      onChange =
        "pidof .awesome-wrapped 1>/dev/null && awesome-client 'awesome.restart()' || true ";
    };
    "awesome/scratch.lua".source = pkgs.fetchurl {
      url =
        "https://raw.githubusercontent.com/notnew/awesome-scratch/master/scratch.lua";
      sha256 = "0jskdpfkk4n23skgsa144pnljrgv5rb0gd5jmqlgpzg8729717sd";
    };
    "awesome/sharedtags" = {
      source = pkgs.fetchFromGitHub {
        owner = "Drauthius";
        repo = "awesome-sharedtags";
        rev = "32d878d0d12bcfd900f2c6a7516a1370e6ebb7d6";
        sha256 = "0js6v2jmkczi3g8j7vbk9hsq5wfz96jnhi7ka5c1rqw22lmjmlrk";
      };
    };
  };
}
