{ config, inputs, ... }: {
  programs.nix-index.enable = true;
  systemd.user.services.nix-index.Service = {
    ExecStart =
      "${config.programs.nix-index.package}/bin/nix-index -f ${inputs.nixpkgs}";
    ProtectHome = false;
    StandardOutput = "null";
  };
  systemd.user.timers.nix-index = {
    Unit.description = "Index nix store";
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
