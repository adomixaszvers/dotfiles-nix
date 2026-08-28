# https://www.reddit.com/r/KeePass/comments/7txk8w/sync_with_google_drive_in_linux_working_example/
{ pkgs, ... }:
let
  db-name = "NewDatabase.kdbx";
  keepass-trigger-load = pkgs.writeShellScriptBin "keepass_trigger_load.sh" ''
    cd ~/google_drive
    drive pull -ignore-conflict -quiet ${db-name}
  '';
  keepass-trigger-save = pkgs.writeShellScriptBin "keepass_trigger_save.sh" ''
    cp ~/Dokumentai/${db-name} ~/google_drive
    cd ~/google_drive
    drive push -ignore-conflict -quiet ${db-name}
  '';
  my-keepass = pkgs.keepass.override { plugins = [ pkgs.keepass-keepassrpc ]; };
in
{
  home.packages = [
    # keep-sorted start
    keepass-trigger-load
    keepass-trigger-save
    my-keepass
    pkgs.drive
    # keep-sorted end
  ];
}
