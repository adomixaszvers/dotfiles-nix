{ pkgs, ... }: {
  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };
  services.udev.extraRules = # udev
    ''
      ACTION=="remove", ENV{ID_MODEL_ID}=="0407", ENV{ID_VENDOR_ID}=="1050", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
    '';
}
