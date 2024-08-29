{
  security.doas = {
    enable = true;
    extraRules = [
      {
        users = [ "adomas" ];
        keepEnv = true;
      }
    ];
  };
}
