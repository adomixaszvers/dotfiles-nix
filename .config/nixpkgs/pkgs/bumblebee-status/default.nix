{ stdenv, fetchgit, python3 }:
let
  version = "1.7.2";
in stdenv.mkDerivation {
  name = "bumblebee-status-${version}";
  inherit version;
  src = fetchgit {
    rev = "c514d965cb25314f57a3bca1c9643e86af00a214";
    url = "https://github.com/tobi-wan-kenobi/bumblebee-status.git";
    sha256 = "1hppysqkmr6wa9dmx3szb6xs3l6s9dmadnxp6g84m7rbzi875iw0";
  };
  buildInputs = [(python3.withPackages (
  ps: with ps; [i3ipc requests taskw netifaces psutil]
  ))];
  unpackPhase = ":";
  installPhase = ''
    install -d $out/opt/bumblebee-status
    cp -rp $src/* $out/opt/bumblebee-status
    install -d $out/bin
    ln -s $out/opt/bumblebee-status/bumblebee-status $out/bin/bumblebee-status
  '';
  meta = {};
}
