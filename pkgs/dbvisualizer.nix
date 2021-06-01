{ stdenv, lib, fetchurl, jre, makeWrapper }:

stdenv.mkDerivation {
  name = "dbvisualizer-10.0.25";

  src = fetchurl {
    url =
      "https://www.dbvis.com/product_download/dbvis-10.0.25/media/dbvis_unix_10_0_25.tar.gz";
    sha256 = "0f9lqsvyxy0wa6hgdvc9vxvfb44xb46fgcdj5g9y013cj4nsqzf1";
  };

  buildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp -a . $out
    ln -sf $out/dbvis $out/bin
    wrapProgram $out/bin/dbvis --set INSTALL4J_JAVA_HOME ${jre}
  '';

  meta = {
    description = "The universal database tool";
    homepage = "https://www.dbvis.com/";
    license = lib.licenses.unfree;
  };
}
