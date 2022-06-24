{ ani-cli, bash, lib, gnugrep, gnused, ffmpeg, mpv, curl, runCommandLocal
, openssl, makeWrapper }:
runCommandLocal "ani-cli" {
  script = "${ani-cli}/bin/ani-cli";
  nativeBuildInputs = [ makeWrapper ];
} ''
  makeWrapper $script $out/bin/ani-cli \
    --prefix PATH : ${
      lib.makeBinPath [ bash gnugrep gnused ffmpeg mpv curl openssl ]
    }
  install -d $out/lib/ani-cli
  for i in ${ani-cli}/lib/ani-cli/*; do
    install -m444 $i $out/lib/ani-cli/
  done
''
