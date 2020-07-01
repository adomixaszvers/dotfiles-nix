{ stdenv, nivSources }:
stdenv.mkDerivation {
  name = "kakoune-text-objects";
  src = nivSources.kakoune-text-objects;

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
