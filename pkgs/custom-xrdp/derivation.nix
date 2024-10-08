{
  xrdp,
  runCommand,
  buildEnv,
  lib,
}:
let
  ltInis = runCommand "xrdp-lt-inis" { } ''
    install -d $out/etc/xrdp
    cp ${builtins.path { path = ./km-00010427.ini; }} $out/etc/xrdp/km-00010427.ini
    cp ${builtins.path { path = ./xrdp_keyboard.ini; }} $out/etc/xrdp/xrdp_keyboard.ini
  '';
in
buildEnv {
  name = "xrdp-with-lt-keys";
  paths = [
    (lib.lowPrio xrdp)
    ltInis
  ];
}
