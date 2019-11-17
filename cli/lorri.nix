{ pkgs, ... }:
let lorri = pkgs.lorri;
in {
  home.packages = [ lorri ];
  programs.direnv = {
    stdlib = ''
      use_nix() {
        eval "$(lorri direnv)"
      }
    '';
  };
  systemd.user.sockets.lorri = {
    Unit = { Description = "lorri build daemon"; };
    Socket = { ListenStream = "%t/lorri/daemon.socket"; };
    Install = { WantedBy = [ "sockets.target" ]; };
  };
  systemd.user.services.lorri =
    let path = with pkgs; lib.makeBinPath [ nix gnutar git mercurial ];
    in {
      Unit = {
        Description = "lorri build daemon";
        Documentation = "https://github.com/target/lorri";
        ConditionUser = "!@system";
        Requires = "lorri.socket";
        After = "lorri.socket";
        RefuseManualStart = true;
      };

      Service = {
        ExecStart = "${lorri}/bin/lorri daemon";
        PrivateTmp = true;
        ProtectSystem = "strict";
        WorkingDirectory = "%h";
        Restart = "on-failure";
        Environment = "PATH=${path} RUST_BACKTRACE=1";
      };
    };
}
