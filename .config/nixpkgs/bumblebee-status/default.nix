{ stdenv, fetchgit, python3 }:
let
  version = "1.7.0";
in stdenv.mkDerivation {
  name = "bumblebee-status-${version}";
  version = "1.7.0";
  src = fetchgit {
    rev = "7152bb17f2536edd807b143517073205369d51f7";
    url = "https://github.com/tobi-wan-kenobi/bumblebee-status.git";
    sha256 = "11vv4s62zmmnaygg09j1sas79qjiigajym7xs3x4ghp0ahkacsm7";
  };
  buildInputs = [(python3.withPackages (
  ps: with ps; [i3ipc requests taskw netifaces psutil]
  ))];
  unpackPhase = ":";
  installPhase = ''
    install -m755 -D $src/bumblebee-status $out/bin/bumblebee-status
    cp -r $src/bumblebee $out/bin/bumblebee
    cp -r $src/themes $out/bin/themes
    install -d $out/share/doc
    install -m655 -D $src/CODE_OF_CONDUCT.md $src/CONTRIBUTING.md $src/README.md $src/LICENSE $out/share/doc
  '';
  meta = {};
}
