{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "kakoune-sudo-write";
  version = "2019-01-24";
  src = fetchFromGitHub {
    owner = "occivink";
    repo = "kakoune-sudo-write";
    rev = "9a11b5c8c229c517848c069e3614f591a43d6ad3";
    sha256 = "0ga8048z7hkfpl1gjrw76d4dk979a6vjpg8jsnrs3swg493qr6ix";
  };

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp $src/sudo-write.kak $out/share/kak/autoload/plugins/sudo-write.kak
  '';

  meta = with stdenv.lib; {
    description = "Write to files using 'sudo'";
    homepage = "https://github.com/occivink/kakoune-sudo-write";
    license = licenses.unlicense;
    platform = platforms.all;
  };
}
