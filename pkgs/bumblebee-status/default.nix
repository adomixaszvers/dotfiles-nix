{ stdenv, fetchFromGitHub, python3 }:
let version = "1.9.0";
in stdenv.mkDerivation {
  name = "bumblebee-status-${version}";
  inherit version;
  src = fetchFromGitHub {
    owner = "tobi-wan-kenobi";
    repo = "bumblebee-status";
    rev = "v${version}";
    sha256 = "1df59aj6kg93sgagxh018hv8fxrmcvnkz51lvpvsdibc2xx0irn4";
  };
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
  meta = with stdenv.lib; {
    platforms = platforms.linux;
    license = licenses.mit;
    description =
      "a modular, theme-able status line generator for the i3 window manager";
    homepage = "https://github.com/tobi-wan-kenobi/bumblebee-status";
  };
}
