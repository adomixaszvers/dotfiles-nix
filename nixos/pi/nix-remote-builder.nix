{
  # Allow more open files for non-root users to run NixOS VM tests.
  security.pam.loginLimits = [
    {
      domain = "*";
      item = "nofile";
      type = "-";
      value = "20480";
    }
  ];

  # Give restricted SSH access to the build scheduler
  users.users.nix-remote-builder = {
    isNormalUser = true;
    group = "nogroup";
    openssh.authorizedKeys.keyFiles = [ ../keys/work-build-scheduler.pub ];
  };
  nix.settings.trusted-users = [ "nix-remote-builder" ];

  # Allow more nix-daemon sessions to connect at the same time.
  services.openssh.settings.MaxStartups = 100;
}
