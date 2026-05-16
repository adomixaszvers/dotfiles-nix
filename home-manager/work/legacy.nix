{ pkgs, inputs, ... }:
{
  home = {
    file = {
      "jdks/oraclejdk8".source = pkgs.callPackage (import ./oraclejdk-linux-base.nix {
        productVersion = "8";
        patchVersion = "202";
        jceName = "jce_policy-8.zip";
        sha256JCE = "19n5wadargg3v8x76r7ayag6p2xz1bwhrgdzjs9f4i6fvxz9jr4w";
        sha256.x86_64-linux = "1q4l8pymjvsvxfwaw0rdcnhryh1la2bvg5f4d4my41ka390k4p4s";
      }) { };
      "nodejs/12".source =
        let
          # nixos-20.09
          oldNixpkgs = builtins.getFlake "github:NixOS/nixpkgs/1c1f5649bb9c1b0d98637c8c365228f57126f361";
          oldPkgs = builtins.getAttr pkgs.stdenv.hostPlatform.system oldNixpkgs.legacyPackages;
        in
        oldPkgs.nodejs-12_x;
    };
    packages =
      let
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
      in
      [
        eclipse-activiti
        (pkgs.callPackage ./sqldeveloper {
          jdk = pkgs.openjdk17.override { enableJavaFX = true; };
        })
      ];
    sessionVariables = {
      CS_AUTH_KEYS = "/home/adomas/HSMrsa.pub";
      CS_PKCS11_R2_CFG = "/home/adomas/cs_pkcs11_R2.cfg";
      CRYPTOSERVER = "3001@localhost";
    };
  };
}
