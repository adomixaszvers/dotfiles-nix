{ ani-cli, bash, lib, gnugrep, gnused, ffmpeg, mpv, curl, runCommandLocal
, makeWrapper }:
runCommandLocal "ani-cli" {
  script = "${ani-cli}/ani-cli";
  nativeBuildInputs = [ makeWrapper ];
} ''
  makeWrapper $script $out/bin/ani-cli \
    --prefix PATH : ${lib.makeBinPath [ bash gnugrep gnused ffmpeg mpv curl ]}
''
