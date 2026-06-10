{ withSystem, ... }:
{
  flake.packages.x86_64-linux = withSystem "x86_64-linux" (
    { pkgs, ... }:
    {
      # does not work because it depends on OPENSSL_1.0.0
      # dokobit-plugin = pkgs.callPackage ./dokobit-plugin { };
      bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
      bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
      hunspell-lt = pkgs.callPackage ./hunspell-lt { };
      jj-watch = pkgs.callPackage ./jj-watch.nix { };
      kaknix = pkgs.callPackage ./kaknix.nix { };
      maimpick = pkgs.callPackage ./maimpick.nix { };
      # mcard-toolbox = pkgs.callPackage ./mcard-toolbox { };
      networkmanager-vpnc = pkgs.callPackage ./networkmanager-vpnc { };
      niri-swap-monitors = pkgs.callPackage ./niri-swap-monitors.nix { };
      restart-eww = pkgs.callPackage ./restart-eww.nix { };
      rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
      shikane = pkgs.shikane.overrideAttrs (
        finalAttrs: _previousAttrs: {
          version = "1.0.1";
          src = pkgs.fetchFromGitLab {
            owner = "w0lff";
            repo = "shikane";
            rev = "v${finalAttrs.version}";
            hash = "sha256-Chc1+JUHXzuLl26NuBGVxSiXiaE4Ns1FXb0dBs6STVk=";
          };
          cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
            inherit (finalAttrs) pname version src;
            hash = "sha256-eVEfuX/dNFoNH9o18fIx51DP/MWrQMqInU4wtGCmUbQ=";
          };
        }
      );
      sxhkd = pkgs.sxhkd.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [ ./sxhkd.patch ];
      });
      sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
      toggle-touchpad = pkgs.callPackage ./toggle-touchpad.nix { };
      tail-volume = pkgs.callPackage ./tail-volume { };
    }
  );
  perSystem =
    {
      pkgs,
      ...
    }:
    {
      packages = {
        he = pkgs.callPackage ./he.nix { };
        hm-repl = pkgs.callPackage ./hm-repl.nix { };
      };
    };
}
