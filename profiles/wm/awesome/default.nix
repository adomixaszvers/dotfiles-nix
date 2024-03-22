{ pkgs, ... }: {
  imports = [ ../picom.nix ];
  services.pasystray.enable = true;
  xsession.windowManager.awesome = { enable = true; };
  xdg.configFile = let
    copycats = pkgs.fetchGitHub {
      owner = "lcpz";
      repo = "awesome-copycats";
      rev = "16c16bb16eb1f2d272d6bd85872a05c8c958aeb6";
      hash = "sha256-8CGjbUpfyj2eOu+r87gyOG03FBtY4tvUgMwCl9ohT9E=";
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
        # branch = "v4.0";
        owner = "Drauthius";
        repo = "awesome-sharedtags";
        rev = "346ec10905a38c7a68504ccc3322fb98f95d70fc";
        hash = "sha256-Aqf44l3efSbABobWXdC3w34lhHMMzbBFZyfw0Ix/7mw=";
      };
    };
  };
}
