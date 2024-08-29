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
  version = "1.4.0.0";
  src = fetchzip {
    stripRoot = false;
    url = "https://www.nsc.vrm.lt/files/Toolbox_LT-${finalAttrs.version}.zip";
    hash = "sha256-S8+MmIaDnbP8IYInK7JPsHpptSB1ZyoAbj1Ht5W01yo=";
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
    bsdtar xf $src/$pname-amd64-$version.deb
    bsdtar xf data.tar.xz
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
