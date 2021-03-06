{ stdenv, lib, kakoune-sudo-write-source }:
stdenv.mkDerivation {
  name = "kakoune-sudo-write";
  src = kakoune-sudo-write-source;

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp $src/sudo-write.kak $out/share/kak/autoload/plugins/sudo-write.kak
  '';

  meta = with lib; {
    description = "Write to files using 'sudo'";
    homepage = "https://github.com/occivink/kakoune-sudo-write";
    license = licenses.unlicense;
    platform = platforms.all;
  };
}
