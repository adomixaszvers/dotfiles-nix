{ stdenv, inputs }:
stdenv.mkDerivation {
  name = "kakoune-sudo-write";
  src = inputs.kakoune-sudo-write;

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
