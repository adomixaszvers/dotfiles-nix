{
  stdenv,
  lib,
  pcsclite,
  libarchive,
  qt5,
  fetchurl,
  autoPatchelfHook,
  openssl,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "dokobit-plugin";
  version = "1.3.22.0";
  src = fetchurl {
    url = "https://github.com/dokobit/browser-plugin/raw/master/Linux/64Bit/dokobit-plugin-en_${finalAttrs.version}.deb";
    sha256 = "1xwd2g4ix0ym7g6rfcynnfg8dv4pjy73g35985hxmxc0gwzzsvc1";
  };
  buildInputs = [
    pcsclite
    qt5.qtbase
    openssl
  ];
  nativeBuildInputs = [
    libarchive
    qt5.wrapQtAppsHook
    autoPatchelfHook
  ];
  unpackPhase = ''
    bsdtar xf $src
    bsdtar xf data.tar.xz
  '';
  installPhase = ''
    mkdir $out
    cp -av usr/{bin,lib,share} $out
    substituteInPlace $out/lib/mozilla/native-messaging-hosts/lt.isign.chromesigning.json \
      $out/share/dokobit-plugin/lt.isign.chromesigning.json \
      --replace-fail /usr/bin $out/bin
  '';
  meta = {
    license = lib.licenses.unfree;
  };
})
