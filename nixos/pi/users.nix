{ pkgs, config, ... }: {
  sops.secrets."pi/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  sops.secrets."root/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  users = {
    mutableUsers = false;
    # put your own configuration here, for example ssh keys:
    users.root = {
      passwordFile = config.sops.secrets."root/password".path;
      openssh.authorizedKeys.keyFiles = [ ../keys/yubikey.pub ];
    };
    extraUsers.pi = {
      passwordFile = config.sops.secrets."pi/password".path;
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "video" "wheel" ];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keyFiles =
        [ ../keys/juice_ed25519.pub ../keys/laptop.pub ../keys/yubikey.pub ];
    };
  };
}
