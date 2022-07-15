{ pkgs, lib, system, ... }:
{
  programs.nix-index.enable = true;
} // (lib.mkIf (system == "x86_64-linux") {
  systemd.user.services.update-nix-index.Service = {
    ExecStart = (pkgs.writers.writeDash "update-nix-index-cache" ''
      filename="index-x86_64-linux"
      mkdir -p ~/.cache/nix-index
      cd ~/.cache/nix-index
      # -N will only download a new version if there is an update.
      wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename
      ln -f $filename files
    '').outPath;
    ProtectHome = false;
    Environment = "PATH=${lib.makeBinPath (with pkgs; [ wget coreutils ])}";
  };
  systemd.user.timers.update-nix-index = {
    Unit.Description = "Index nix store";
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
})
