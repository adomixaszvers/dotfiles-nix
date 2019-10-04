{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "kakoune-text-objects";
  version = "2019-05-09";
  src = fetchFromGitHub {
    owner = "Delapouite";
    repo = "kakoune-text-objects";
    rev = "1ef64a04e183e27f87655db7a334c2eea810bfbf";
    sha256 = "1v5a383kqwl4kzbi4s5n22a2cw9fim2rk08a0430k1qm1h8l25mi";
  };

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp text-objects.kak $out/share/kak/autoload/plugins
  '';

  meta = with stdenv.lib; {
    description = "kakoune plugin providing extra text-objects";
    homepage = "https://github.com/Delapouite/kakoune-text-objects";
    license = licenses.publicDoman;
    platform = platforms.all;
  };
}
