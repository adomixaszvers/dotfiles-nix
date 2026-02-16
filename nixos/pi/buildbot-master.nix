{
  inputs,
  config,
  ...
}:
let
  sopsFile = ./secrets/buildbot.yaml;
  secretConf = {
    inherit sopsFile;
    owner = config.users.users.buildbot.name;
  };
in
{
  imports = [
    inputs.buildbot-nix.nixosModules.buildbot-master
    ../github-hosts.nix
  ];

  networking.firewall.interfaces.wg0.allowedTCPPorts = [ 9989 ];

  sops.secrets."buildbot/oauth-secret" = secretConf;
  sops.secrets."buildbot/webhook-secret" = secretConf;
  sops.secrets."buildbot/token" = secretConf;
  sops.secrets."buildbot/cachix-signing-key" = secretConf;
  sops.secrets."buildbot/ssh-key" = secretConf;
  sops.secrets."buildbot-nix/work-worker-password" = {
    sopsFile = ../common-secrets/buildbot/work.yaml;
    owner = config.users.users.buildbot.name;
  };
  sops.templates."buildbot-nix/workers.json".content = ''
    [
      { "name": "darbas", "pass": "${
        config.sops.placeholder."buildbot-nix/work-worker-password"
      }", "cores": 4 }
    ]
  '';
  services.buildbot-nix.master = {
    enable = true;
    # Domain name under which the buildbot frontend is reachable
    domain = "buildbot.rpi4.beastade.top";
    # The workers file configures credentials for the buildbot workers to connect to the master.
    # "name" is the configured worker name in services.buildbot-nix.worker.name of a worker
    # (defaults to the hostname of the machine)
    # "pass" is the password for the worker configured in `services.buildbot-nix.worker.workerPasswordFile`
    # "cores" is the number of cpu cores the worker has.
    # The number must match the actual core count of the machine as otherwise not enough buildbot-workers are created.
    workersFile = config.sops.templates."buildbot-nix/workers.json".path;
    # Users in this list will be able to reload the project list.
    # All other user in the organization will be able to restart builds or evaluations.
    authBackend = "gitea";
    admins = [ "adomas" ];
    # github = {
    #   # GitHub App configuration
    #   appId = 0; # FIXME: replace with App ID obtained from GitHub
    #   appSecretKeyFile = pkgs.writeText "app-secret.key" "00000000000000000000"; # FIXME: replace with App secret key obtained from GitHub
    #   # A random secret used to verify incoming webhooks from GitHub
    #   # buildbot-nix will set up a webhook for each project in the organization
    #   webhookSecretFile = pkgs.writeText "webhookSecret" "00000000000000000000"; # FIXME: replace this with a secret not stored in the nix store
    #   # Either create a GitHub app or an OAuth app
    #   # After creating the app, press "Generate a new client secret" and fill in the client ID and secret below
    #   oauthId = "aaaaaaaaaaaaaaaaaaaa";
    #   oauthSecretFile = pkgs.writeText "oauthSecret" "ffffffffffffffffffffffffffffffffffffffff"; # FIXME: replace this with a secret not stored in the nix store
    #   # All github projects with this topic will be added to buildbot.
    #   # One can trigger a project scan by visiting the Builds -> Builders page and looking for the "reload-github-project" builder.
    #   # This builder has a "Update Github Projects" button that everyone in the github organization can use.
    #   topic = "buildbot-mic92";
    # };

    # Gitea example
    # authBackend = "gitea"; # login with gitea
    gitea = {
      enable = true;
      instanceUrl = "https://git.rpi4.beastade.top";
      # Create a Gitea App with for redirect uris: https://buildbot.clan.lol/auth/login
      oauthId = "4cf00734-7279-404b-aa0f-c5f7a6411aba";
      oauthSecretFile = config.sops.secrets."buildbot/oauth-secret".path;
      webhookSecretFile = config.sops.secrets."buildbot/webhook-secret".path;
      tokenFile = config.sops.secrets."buildbot/token".path; # replace this with a secret not stored in the nix store
      topic = "buildbot-nix";
    };
    # optional expose latest store path as text file
    # outputsPath = "/var/www/buildbot/nix-outputs";
    pullBased = {
      pollInterval = 600; # 10 minutes
      repositories = {
        dotfiles-nix-gh = {
          url = "git@github.com:adomixaszvers/dotfiles-nix.git";
          defaultBranch = "update_flake_lock_action";
          sshPrivateKeyFile = config.sops.secrets."buildbot/ssh-key".path;
        };
      };
    };

    # optional nix-eval-jobs settings
    buildSystems = [
      "x86_64-linux"
      "aarch64-linux"
    ];
    evalWorkerCount = 2; # limit number of concurrent evaluations
    evalMaxMemorySize = 4096; # limit memory usage per evaluation

    # optional cachix
    cachix = {
      enable = true;
      name = "adomixaszvers";
      # One of the following is required:
      auth.signingKey.file = config.sops.secrets."buildbot/cachix-signing-key".path;
      # auth.authToken.file = "/var/lib/secrets/cachix-token";
    };

    # By default we only build the "default" branch. Using the branches option, buildbot will also build other branches.
    # branches = {
    #   releaseBranches.matchGlob = "release-*";
    # };

    # Generic OIDC Example.
    # authBackend = "oidc";
    # oidc = {
    #  name = "Provider Name";
    #  # URL for the OIDC discovery endpoint
    #  # For Keycloak, this would be https://keycloak.thalheim.io/realms/{realm-name}/.well-known/openid-configuration.
    #  # For PocketID, this would be https://id.thalheim.io/.well-known/openid-configuration.
    #  discoveryUrl = "https://id.thalheim.io/.well-known/openid-configuration";
    #  clientId = "aaaaaaaaaaaaaaaaaaaa";
    #  clientSecretFile = pkgs.writeText "oidc-secret" "0000000000000000000000000000000000000000"; # FIXME: replace this with a secret not stored in the nix store
    #  # Additionally, to set a custom scope and mapping for values, see the master module definition
    # };
    # Allow unauthenticated users to perform control actions (cancel, restart, force builds).
    # Useful when running buildbot behind a VPN or on a local network.
    # allowUnauthenticatedControl = true;
  };

  # Optional: Enable acme/TLS in nginx (recommended)
  services.nginx.virtualHosts.${config.services.buildbot-nix.master.domain} = {
    forceSSL = true;
    useACMEHost = "rpi4.beastade.top";
  };

  # Optional: If buildbot is setup to run behind another proxy that does TLS
  # termination set this to true to have buildbot use https:// for its endpoint
  #useHTTPS = true;
}
