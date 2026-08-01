{ config, ... }: {
  services.harmonia.cache = {
    enable = true;
    # FIXME: generate a public/private key pair like this:
    # $ nix-store --generate-binary-cache-key cache.yourdomain.tld-1 /var/lib/secrets/harmonia.secret /var/lib/secrets/harmonia.pub
    # services.harmonia.signKeyPaths = [ "/var/lib/secrets/harmonia.secret" ];
    # Example using sops-nix to store the signing key
    signKeyPaths = [ config.sops.secrets.harmonia-key.path ];
    settings.priority = 30;
  };
  sops.secrets.harmonia-key = {
    sopsFile = ./secrets/harmonia.secret;
    format = "binary";
  };

  # optional if you use allowed-users in other places
  #nix.settings.allowed-users = [ "harmonia" ];

  networking.firewall.allowedTCPPorts = [
    443
    80
  ];

  services.nginx = {
    virtualHosts."cache.bl.beastade.top" = {
      forceSSL = true;
      useACMEHost = "bl.beastade.top";

      locations."/".extraConfig = ''
        proxy_pass http://127.0.0.1:5000;
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };
}
