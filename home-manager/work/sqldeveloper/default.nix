{
  lib,
  stdenv,
  makeDesktopItem,
  makeWrapper,
  fetchurl,
  unzip,
  jdk,
}:

let
  desktopItem = makeDesktopItem {
    name = "sqldeveloper";
    exec = "sqldeveloper";
    icon = "sqldeveloper";
    desktopName = "Oracle SQL Developer";
    genericName = "Oracle SQL Developer";
    comment = "Oracle's Oracle DB GUI client";
    categories = [ "Development" ];
  };
in
stdenv.mkDerivation (finalAttrs: {

  version = "26.2.0.186.2220";
  pname = "sqldeveloper";

  src = fetchurl {
    url = "https://download.oracle.com/otn_software/java/sqldeveloper/sqldeveloper-${finalAttrs.version}-no-jre.zip";
    sha256 = "0zfvzhqlyrzfpj4mw2n3vmd7vnn7qradgpq32nkhkpxnn1rw0hk9";
  };

  nativeBuildInputs = [
    makeWrapper
    unzip
  ];

  unpackCmd = "unzip $curSrc";

  installPhase = ''
    mkdir -p $out/libexec $out/share/{applications,pixmaps}
    mv * $out/libexec/

    mv $out/libexec/icon.png $out/share/pixmaps/sqldeveloper.png
    cp ${desktopItem}/share/applications/* $out/share/applications
    echo 'AddLinuxVM9OrHigherOption --add-exports=java.desktop/com.sun.java.swing.plaf.gtk=ALL-UNNAMED' >> $out/libexec/ide/bin/jdk.conf

    makeWrapper $out/libexec/sqldeveloper/bin/sqldeveloper $out/bin/sqldeveloper \
      --set JAVA_HOME ${jdk.home} \
      --chdir "$out/libexec/sqldeveloper/bin"
  '';

  meta = {
    description = "Oracle's Oracle DB GUI client";
    longDescription = ''
      Oracle SQL Developer is a free integrated development environment that
      simplifies the development and management of Oracle Database in both
      traditional and Cloud deployments. SQL Developer offers complete
      end-to-end development of your PL/SQL applications, a worksheet for
      running queries and scripts, a DBA console for managing the database,
      a reports interface, a complete data modeling solution, and a migration
      platform for moving your 3rd party databases to Oracle.
    '';
    homepage = "http://www.oracle.com/technetwork/developer-tools/sql-developer/overview/";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
    maintainers = [ lib.maintainers.ardumont ];
  };
})
