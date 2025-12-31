{
  stdenv,
  lib,
  pcsclite,
  libarchive,
  qt5,
  fetchzip,
  autoPatchelfHook,
  openjpeg,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "mcard-toolbox";
  version = "1.6.0.0";
  src = fetchzip {
    stripRoot = false;
    url = "https://www.nsc.vrm.lt/files/Toolbox_LT-${finalAttrs.version}.zip";
    hash = "sha256-RQ+5+0XhUFC1CWg0zhCRAur6pSZ+xOi11aTnqm+1cFA=";
  };
  buildInputs = [
    pcsclite
    qt5.qtbase
    openjpeg
  ];
  nativeBuildInputs = [
    libarchive
    qt5.wrapQtAppsHook
    autoPatchelfHook
  ];
  unpackPhase = ''
    bsdtar xf $src/$pname-lt-amd64.deb
    bsdtar xf data.tar.zst
  '';
  installPhase = ''
    mkdir $out
    cp -av usr/{bin,lib,share} $out
    substituteInPlace $out/share/applications/lt.softemia.mcard-toolbox.desktop --replace-fail /usr/bin $out/bin --replace-fail /usr/share $out/share
  '';
  meta = {
    license = lib.licenses.unfree;
  };
})
