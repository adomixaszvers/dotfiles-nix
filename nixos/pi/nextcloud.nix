{ pkgs, config, ... }:
{
  sops = {
    secrets =
      let
        sopsFile = ./secrets/nextcloud.yaml;
      in
      {
        "nextcloud/adminpass" = {
          owner = "nextcloud";
          inherit sopsFile;
        };
        "nextcloud/minioSecret" = {
          owner = "nextcloud";
          inherit sopsFile;
        };
      };
    templates."minio-credentials.env" = {
      content = ''
        MINIO_ROOT_USER=nextcloud
        MINIO_ROOT_PASSWORD=${config.sops.placeholder."nextcloud/minioSecret"}
      '';
    };
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
          enable = true;
          bucket = "nextcloud";
          verify_bucket_exists = false;
          key = "nextcloud";
          secretFile = config.sops.secrets."nextcloud/minioSecret".path;
          useSsl = false;
          port = 9000;
          usePathStyle = true;
          region = "us-east-1";
          hostname = "localhost";
        };
      };
      settings.enabledPreviewProviders = [
        "OC\\Preview\\BMP"
        "OC\\Preview\\GIF"
        "OC\\Preview\\JPEG"
        "OC\\Preview\\Krita"
        "OC\\Preview\\MarkDown"
        "OC\\Preview\\MP3"
        "OC\\Preview\\OpenDocument"
        "OC\\Preview\\PNG"
        "OC\\Preview\\TXT"
        "OC\\Preview\\XBitmap"
        "OC\\Preview\\HEIC"
      ];
      hostName = "nextcloud.rpi4.beastade.top";
      https = true;
      configureRedis = true;
    };

    minio = {
      enable = true;
      listenAddress = "127.0.0.1:9000";
      consoleAddress = "127.0.0.1:9001";
      rootCredentialsFile = config.sops.templates."minio-credentials.env".path;
    };

    postgresql.package = pkgs.postgresql_16;

    nginx.virtualHosts.${config.services.nextcloud.hostName} = {
      useACMEHost = "rpi4.beastade.top";
      forceSSL = true;
    };
  };
}
