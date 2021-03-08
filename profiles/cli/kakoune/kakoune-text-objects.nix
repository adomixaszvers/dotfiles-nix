{ stdenv, kakoune-text-objects-source }:
stdenv.mkDerivation {
  name = "kakoune-text-objects";
  src = kakoune-text-objects-source;

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
