# https://www.reddit.com/r/KeePass/comments/7txk8w/sync_with_google_drive_in_linux_working_example/
{ pkgs, ... }:
with pkgs;
let
  db-name = "NewDatabase.kdbx";
  keepass-trigger-load = writeShellScriptBin "keepass_trigger_load.sh" ''
    cd ~/google_drive
    drive pull -ignore-conflict -quiet ${db-name}
  '';
  keepass-trigger-save = writeShellScriptBin "keepass_trigger_save.sh" ''
    cp ~/Dokumentai/${db-name} ~/google_drive
    cd ~/google_drive
    drive push -ignore-conflict -quiet ${db-name}
  '';
  my-keepass = keepass.override { plugins = [ keepass-keepassrpc ]; };
in { home.packages = [ drive my-keepass keepass-trigger-load keepass-trigger-save ]; }
