{ config, lib, ... }:
let
  cfg = config.services.forgejo;
  srv = cfg.settings.server;
in
{
  networking.extraHosts = ''
    127.0.0.1 git.rpi4.beastade.top
    10.6.0.6 buildbot.l15.beastade.top
  '';
  services.nginx = {
    virtualHosts.${cfg.settings.server.DOMAIN} = {
      forceSSL = true;
      useACMEHost = "rpi4.beastade.top";
      extraConfig = ''
        client_max_body_size 512M;
      '';
      locations."/".proxyPass = "http://localhost:${toString srv.HTTP_PORT}";
    };
  };

  services.forgejo = {
    enable = true;
    database.type = "postgres";
    # Enable support for Git Large File Storage
    lfs.enable = true;
    settings = {
      server = {
        DOMAIN = "git.rpi4.beastade.top";
        # You need to specify this to remove the port from URLs in the web UI.
        ROOT_URL = "https://${srv.DOMAIN}/";
        HTTP_PORT = 3000;
        SSH_PORT = lib.head config.services.openssh.ports;
      };
      webhook.ALLOWED_HOST_LIST = "*.beastade.top";
      # You can temporarily allow registration to create an admin user.
      service.DISABLE_REGISTRATION = true;
      # Add support for actions, based on act: https://github.com/nektos/act
      actions = {
        ENABLED = false;
        DEFAULT_ACTIONS_URL = "github";
      };
    };
  };
}
