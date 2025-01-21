{ inputs, withSystem, ... }:
{
  flake.packages.x86_64-linux = withSystem "x86_64-linux" (
    { pkgs, ... }:
    {
      # does not work because it depends on OPENSSL_1.0.0
      # dokobit-plugin = pkgs.callPackage ./dokobit-plugin { };
      eclipse-activiti = pkgs.eclipses.eclipseWithPlugins {
        eclipse =
          pkgs.callPackage "${inputs.nixpkgs}/pkgs/applications/editors/eclipse/build-eclipse.nix"
            {
              jdk = pkgs.jdk8;
              gtk = pkgs.gtk2;
            }
            {
              name = "eclipse-java-4.4.2";
              description = "Eclipse IDE for Java Developers";
              productVersion = "4.4.2";
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
      oraclejdk8 = pkgs.callPackage (import ./oraclejdk-linux-base.nix {
        productVersion = "8";
        patchVersion = "202";
        jceName = "jce_policy-8.zip";
        sha256JCE = "19n5wadargg3v8x76r7ayag6p2xz1bwhrgdzjs9f4i6fvxz9jr4w";
        sha256.x86_64-linux = "1q4l8pymjvsvxfwaw0rdcnhryh1la2bvg5f4d4my41ka390k4p4s";
      }) { };
      mcard-toolbox = pkgs.callPackage ./mcard-toolbox { };
      sqldeveloper = pkgs.callPackage ./sqldeveloper {
        jdk = pkgs.openjdk17.override { enableJavaFX = true; };
      };
      soapui =
        let
          version = "5.6.1";
        in
        pkgs.soapui.overrideAttrs (_old: {
          inherit version;
          src = pkgs.fetchurl {
            url = "https://s3.amazonaws.com/downloads.eviware/soapuios/${version}/SoapUI-${version}-linux-bin.tar.gz";
            sha256 = "sha256-c13iRcceMtLILlw6ockB9bMf7EVBeAATFy/Ln5ezy3c=";
          };
        });
    }
  );
  perSystem =
    {
      pkgs,
      inputs',
      system,
      ...
    }:
    let
      nixCatsBuilder = import ../profiles/cli/neovim/nixCatsBuilder.nix {
        inherit (inputs) nixpkgs nixCats;
        inherit system;
      };
    in
    {
      packages = {
        bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
        bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
        hunspell-lt = pkgs.callPackage ./hunspell-lt { };
        he = pkgs.callPackage ./he.nix { };
        hypr-window-select = pkgs.callPackage ./hypr-window-select.nix { };
        hypr-greedy-focus = pkgs.callPackage ./hypr-greedy-focus.nix { };
        hm-option = pkgs.callPackage ./hm-option.nix { };
        hm-repl = pkgs.callPackage ./hm-repl.nix { };
        hm-switch = pkgs.callPackage ./hm-switch.nix {
          inherit (inputs'.home-manager.packages) home-manager;
        };
        kaknix = pkgs.callPackage ./kaknix.nix { };
        maimpick = pkgs.callPackage ./maimpick.nix { };
        neovim = nixCatsBuilder "nixCats";
        neovim-nix = nixCatsBuilder "nixCats-small";
        restart-eww = pkgs.callPackage ./restart-eww.nix { };
        rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
        sxhkd = pkgs.sxhkd.overrideAttrs (old: {
          patches = old.patches ++ [ ./sxhkd.patch ];
        });
        sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
        toggle-touchpad = pkgs.callPackage ./toggle-touchpad.nix { };
        tail-volume = pkgs.callPackage ./tail-volume { };
      };
    };
}
