{
  programs.firefox = {
    enable = true;
    policies = {
      DisablePocket = true;
      DisableTelemetry = true;
      PasswordManagerEnabled = false;
    };
  };
}
