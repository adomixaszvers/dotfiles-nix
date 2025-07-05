{ pkgs, config, ... }:
{
  sops.secrets =
    let
      sopsFile = ./secrets/nextcloud.yaml;
    in
    {
      "nextcloud/adminpass" = { inherit sopsFile; };
      "nextcloud/minioAdminCredentials" = { inherit sopsFile; };
      "nextcloud/minioSecret" = { inherit sopsFile; };
    };

  services = {
    nextcloud = {
      enable = true;
      package = pkgs.nextcloud31;
      database.createLocally = true;
      config = {
        adminpassFile = config.sops.secrets."nextcloud/adminpass".path;
        dbtype = "pgsql";
        objectstore.s3 = {
          bucket = "nextcloud";
          verify_bucket_exists = true;
          key = "nextcloud";
          secretFile = config.sops.secrets."nextcloud/minioSecret".path;
          useSsl = false;
          port = 9000;
          usePathStyle = true;
          region = "us-east-1";
          hostname = "localhost";
        };
      };
      hostName = "nextcloud.lan.beastade.top";
      https = true;
      configureRedis = true;
    };

    minio = {
      enable = true;
      listenAddress = "127.0.0.1:9000";
      consoleAddress = "127.0.0.1:9001";
      rootCredentialsFile = config.sops.secrets."nextcloud/minioAdminCredentials".path;
    };

    nginx.virtualHosts.${config.services.nextcloud.hostName} = {
      useACMEHost = "lan.beastade.top";
      forceSSL = true;
    };
  };
}
