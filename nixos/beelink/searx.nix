{ config, ... }:
{
  sops.secrets."searx/secret_key".sopsFile = ./secrets/searx.yaml;
  # adapter from https://wiki.nixos.org/wiki/SearXNG
  services.searx = {
    enable = true;
    redisCreateLocally = true;

    # UWSGI configuration
    configureUwsgi = true;

    uwsgiConfig = {
      socket = "/run/searx/searx.sock";
      http = ":8888";
      chmod-socket = "660";
    };

    # Searx configuration
    settings = {
      # Instance settings
      general = {
        debug = false;
        instance_name = "SearXNG Instance";
        donation_url = false;
        contact_url = false;
        privacypolicy_url = false;
        enable_metrics = false;
      };

      # User interface
      ui = {
        static_use_hash = true;
        default_locale = "en";
        query_in_title = true;
        infinite_scroll = false;
        center_alignment = true;
        default_theme = "simple";
        theme_args.simple_style = "auto";
        search_on_category_select = false;
        hotkeys = "vim";
      };

      # Search engine settings
      search = {
        safe_search = 2;
        autocomplete_min = 2;
        autocomplete = "duckduckgo";
      };

      # Server configuration
      server = {
        base_url = "https://searx.bl.beastade.top";
        port = 8888;
        bind_address = "127.0.0.1";
        secret_key = config.sops.secrets."searx/secret_key".path;
        limiter = false;
        public_instance = false;
        image_proxy = true;
        method = "GET";
      };

      # Outgoing requests
      outgoing = {
        request_timeout = 5.0;
        max_request_timeout = 15.0;
        pool_connections = 100;
        pool_maxsize = 15;
        enable_http2 = true;
      };

      # Enabled plugins
      enabled_plugins = [
        "Basic Calculator"
        "Hash plugin"
        "Tor check plugin"
        "Open Access DOI rewrite"
        "Hostnames plugin"
        "Unit converter plugin"
        "Tracker URL remover"
      ];
    };
  };

  # Systemd configuration
  systemd.services.nginx.serviceConfig.ProtectHome = false;

  # User management
  users.groups.searx.members = [ "nginx" ];

  # Nginx configuration
  services.nginx = {
    virtualHosts = {
      "searx.bl.beastade.top" = {
        useACMEHost = "bl.beastade.top";
        forceSSL = true;
        locations = {
          "/" = {
            extraConfig = ''
              uwsgi_pass unix:${config.services.searx.uwsgiConfig.socket};
            '';
          };
        };
      };
    };
  };
}
