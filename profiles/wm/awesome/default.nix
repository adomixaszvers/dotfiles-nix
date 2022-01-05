{ pkgs, inputs, ... }: {
  imports = [ ../picom.nix ];
  services.pasystray.enable = true;
  xsession.windowManager.awesome = { enable = true; };
  xdg.configFile = let copycats = inputs.awesome-copycats;
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
    "awesome/sharedtags" = { source = inputs.awesome-sharedtags; };
  };
}
