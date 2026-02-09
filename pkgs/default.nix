{ inputs, withSystem, ... }:
let
  nixCatsBuilder =
    system:
    import ../profiles/cli/neovim/nixCatsBuilder.nix {
      inherit (inputs) nixpkgs nixCats;
      inherit system;
    };
in
{
  flake.packages.x86_64-linux = withSystem "x86_64-linux" (
    { pkgs, ... }:
    {
      # does not work because it depends on OPENSSL_1.0.0
      # dokobit-plugin = pkgs.callPackage ./dokobit-plugin { };
      bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
      bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
      eclipse-activiti = pkgs.eclipses.eclipseWithPlugins {
        eclipse =
          pkgs.callPackage "${inputs.nixpkgs}/pkgs/applications/editors/eclipse/build-eclipse.nix"
            {
              jdk = pkgs.jdk8;
              gtk = pkgs.gtk2;
            }
            {
              pname = "eclipse-java";
              description = "Eclipse IDE for Java Developers";
              version = "4.4.2";
              src = pkgs.fetchurl {
                url = "https://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/luna/SR2/eclipse-java-luna-SR2-linux-gtk-x86_64.tar.gz";
                sha256 = "13s88vlqd45yh8vkj51q6nrwxwj8nbss6aaniqg6bl2y43xkizdr";
              };
            };
        plugins = [
          (pkgs.eclipses.plugins.buildEclipseUpdateSite {
            name = "activiti-designer-5.18.0";
            src = pkgs.fetchzip {
              stripRoot = false;
              url = "http://www.activiti.org/designer/archived/activiti-designer-5.18.0.zip";
              hash = "sha256-2VGdwhv9kHIYXjgQ2hrEhm8scJ6IQBjCD3jF3e7UNcY=";
            };
          })
        ];
      };
      hypr-window-select = pkgs.callPackage ./hypr-window-select.nix { };
      hunspell-lt = pkgs.callPackage ./hunspell-lt { };
      jj-watch = pkgs.callPackage ./jj-watch.nix { };
      kaknix = pkgs.callPackage ./kaknix.nix { };
      maimpick = pkgs.callPackage ./maimpick.nix { };
      # mcard-toolbox = pkgs.callPackage ./mcard-toolbox { };
      neovim = nixCatsBuilder pkgs.stdenv.hostPlatform.system "nixCats";
      networkmanager-vpnc = pkgs.callPackage ./networkmanager-vpnc { };
      niri-swap-monitors = pkgs.callPackage ./niri-swap-monitors.nix { };
      restart-eww = pkgs.callPackage ./restart-eww.nix { };
      rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
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
        neovim-nix = nixCatsBuilder pkgs.stdenv.hostPlatform.system "nixCats-small";
      };
    };
}
