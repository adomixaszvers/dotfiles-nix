{ stdenv, fetchFromGitHub, python3 }:
let
  dictFileName = "lt_LT";
  readmeFile = "README";
in stdenv.mkDerivation rec {
  pname = "hunspell-dict-lt";
  version = "1.3.2";
  src = fetchFromGitHub {
    owner = "ispell-lt";
    repo = "ispell-lt";
    rev = "rel-${version}";
    hash = "sha256-TqlEp/TCsbzI+/oxQG2yiUlkci/kbUu+GrAHaYSVsU4=";
  };
  buildInputs = [ python3 ];
  patchPhase = ''
    patchShebangs tools
  '';
  installPhase = ''
    # hunspell dicts
    install -dm755 "$out/share/hunspell"
    install -m644 build/myspell/${dictFileName}.dic "$out/share/hunspell/"
    install -m644 build/myspell/${dictFileName}.aff "$out/share/hunspell/"
    # myspell dicts symlinks
    install -dm755 "$out/share/myspell/dicts"
    ln -sv "$out/share/hunspell/${dictFileName}.dic" "$out/share/myspell/dicts/"
    ln -sv "$out/share/hunspell/${dictFileName}.aff" "$out/share/myspell/dicts/"
    # docs
    install -dm755 "$out/share/doc"
    install -m644 ${readmeFile} $out/share/doc/${pname}.txt
    runHook postInstall
  '';
}
