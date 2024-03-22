{ stdenv, lib, python3, fetchFromGitHub }:
let
  version = "v2.0.5";
  src = fetchFromGitHub {
    owner = "tobi-wan-kenobi";
    repo = "bumblebee-status";
    rev = "227a23fdb539ff4170e747add2d594e860c0c801";
    hash = "sha256-oedFzB/eDn0HKq8+hkv0naXDT5RiTit7Rl0YGTHIG8U=";
  };

in stdenv.mkDerivation {
  name = "bumblebee-status-${version}";
  inherit version src;
  buildInputs = [
    (python3.withPackages
      (ps: with ps; [ i3ipc requests taskw netifaces psutil ]))
  ];
  unpackPhase = ":";
  installPhase = ''
    install -d $out/share/bumblebee-status
    cp -rp $src/* $out/share/bumblebee-status
    install -d $out/bin
    ln -s $out/share/bumblebee-status/bumblebee-status $out/bin/bumblebee-status
  '';
  meta = with lib; {
    platforms = platforms.linux;
    license = licenses.mit;
    description =
      "a modular, theme-able status line generator for the i3 window manager";
    homepage = "https://github.com/tobi-wan-kenobi/bumblebee-status";
  };
}
