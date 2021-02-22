{ xrdp, writeTextDir, buildEnv, lib }:
let
  ltKeysIni = writeTextDir "etc/xrdp/km-00010427.ini"
    (builtins.readFile ./km-00010427.ini);
  xrdpKeyboardIni = writeTextDir "etc/xrdp/xrdp_keyboard.ini"
    (builtins.readFile ./xrdp_keyboard.ini);
in buildEnv {
  name = "xrdp-with-lt-keys";
  paths = [ ltKeysIni (lib.lowPrio xrdp) xrdpKeyboardIni ];
}
