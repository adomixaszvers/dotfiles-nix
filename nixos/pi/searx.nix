{ config, lib, ... }:
{
  sops.secrets."searx/secret_key".sopsFile = ./secrets/searx.yaml;
  # adapter from https://wiki.nixos.org/wiki/SearXNG
  services.searx = {
    enable = true;
    redisCreateLocally = true;

    # Rate limiting
    limiterSettings = {
      real_ip = {
        x_for = 1;
        ipv4_prefix = 32;
        ipv6_prefix = 56;
      };

      botdetection = {
        ip_limit = {
          filter_link_local = true;
          link_token = true;
        };
      };
    };

    # UWSGI configuration
    runInUwsgi = true;

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
        ban_time_on_fail = 5;
        max_ban_time_on_fail = 120;
      };

      # Server configuration
      server = {
        base_url = "https://search.example.com";
        port = 8888;
        bind_address = "127.0.0.1";
        secret_key = config.sops.secrets."searx/secret_key".path;
        limiter = true;
        public_instance = true;
        image_proxy = true;
        method = "GET";
      };

      # Search engines
      engines = lib.mapAttrsToList (name: value: { inherit name; } // value) {
        "duckduckgo".disabled = true;
        "brave".disabled = true;
        "bing".disabled = false;
        "mojeek".disabled = true;
        "mwmbl".disabled = false;
        "mwmbl".weight = 0.4;
        "qwant".disabled = true;
        "crowdview".disabled = false;
        "crowdview".weight = 0.5;
        "curlie".disabled = true;
        "ddg definitions".disabled = false;
        "ddg definitions".weight = 2;
        "wikibooks".disabled = false;
        "wikidata".disabled = false;
        "wikiquote".disabled = true;
        "wikisource".disabled = true;
        "wikispecies".disabled = false;
        "wikispecies".weight = 0.5;
        "wikiversity".disabled = false;
        "wikiversity".weight = 0.5;
        "wikivoyage".disabled = false;
        "wikivoyage".weight = 0.5;
        "currency".disabled = true;
        "dictzone".disabled = true;
        "lingva".disabled = true;
        "bing images".disabled = false;
        "brave.images".disabled = true;
        "duckduckgo images".disabled = true;
        "google images".disabled = false;
        "qwant images".disabled = true;
        "1x".disabled = true;
        "artic".disabled = false;
        "deviantart".disabled = false;
        "flickr".disabled = true;
        "imgur".disabled = false;
        "library of congress".disabled = false;
        "material icons".disabled = true;
        "material icons".weight = 0.2;
        "openverse".disabled = false;
        "pinterest".disabled = true;
        "svgrepo".disabled = false;
        "unsplash".disabled = false;
        "wallhaven".disabled = false;
        "wikicommons.images".disabled = false;
        "yacy images".disabled = true;
        "bing videos".disabled = false;
        "brave.videos".disabled = true;
        "duckduckgo videos".disabled = true;
        "google videos".disabled = false;
        "qwant videos".disabled = false;
        "dailymotion".disabled = true;
        "google play movies".disabled = true;
        "invidious".disabled = true;
        "odysee".disabled = true;
        "peertube".disabled = false;
        "piped".disabled = true;
        "rumble".disabled = false;
        "sepiasearch".disabled = false;
        "vimeo".disabled = true;
        "youtube".disabled = false;
        "brave.news".disabled = true;
        "google news".disabled = true;
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
      "searx.lan.beastade.top" = {
        useACMEHost = "lan.beastade.top";
        forceSSL = true;
        locations = {
          "/" = {
            extraConfig = ''
              uwsgi_pass unix:${config.services.searx.uwsgiConfig.socket};
            '';
          };
        };
      };
      "searx.wg.beastade.top" = {
        useACMEHost = "wg.beastade.top";
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
