{
  # keep-sorted start
  callPackage,
  eclipses,
  fetchurl,
  fetchzip,
  gtk2,
  jdk8,
  nixpkgs,
  # keep-sorted end
}:
eclipses.eclipseWithPlugins {
  eclipse =
    callPackage "${nixpkgs}/pkgs/applications/editors/eclipse/build-eclipse.nix"
      {
        jdk = jdk8;
        gtk = gtk2;
      }
      {
        pname = "eclipse-java";
        description = "Eclipse IDE for Java Developers";
        version = "4.4.2";
        src = fetchurl {
          url = "https://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/luna/SR2/eclipse-java-luna-SR2-linux-gtk-x86_64.tar.gz";
          sha256 = "13s88vlqd45yh8vkj51q6nrwxwj8nbss6aaniqg6bl2y43xkizdr";
        };
      };
  plugins = [
    (eclipses.plugins.buildEclipseUpdateSite {
      name = "activiti-designer-5.18.0";
      src = fetchzip {
        stripRoot = false;
        url = "http://www.activiti.org/designer/archived/activiti-designer-5.18.0.zip";
        hash = "sha256-2VGdwhv9kHIYXjgQ2hrEhm8scJ6IQBjCD3jF3e7UNcY=";
      };
    })
  ];
}
