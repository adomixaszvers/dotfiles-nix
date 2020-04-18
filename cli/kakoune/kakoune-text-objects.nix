{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "kakoune-text-objects";
  version = "2019-05-09";
  src = fetchFromGitHub {
    owner = "Delapouite";
    repo = "kakoune-text-objects";
    rev = "aacd554982ba9a7a77a788817bb8ee2e1b380ee7";
    sha256 = "0qdzl0z32wgw6k4ssy598sdqk2jh13lwvczslzyg4x5d6z8jwhyf";
  };

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp text-objects.kak $out/share/kak/autoload/plugins
  '';

  meta = with stdenv.lib; {
    description = "kakoune plugin providing extra text-objects";
    homepage = "https://github.com/Delapouite/kakoune-text-objects";
    license = licenses.mit;
    platform = platforms.all;
  };
}
