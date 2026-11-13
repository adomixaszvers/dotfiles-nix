{ pkgs, config, ... }: {
  networking.networkmanager.plugins = [ pkgs.networkmanager-libreswan ];

  # The plugin shells out to the libreswan binaries and expects to find
  # /etc/ipsec.conf (pluto is started with `--config /etc/ipsec.conf`) and an
  # /etc/ipsec.secrets that includes /etc/ipsec.d/*.secrets, which is where it
  # writes the pre-shared key.  Only this module provides both; with
  # NetworkManager alone, /etc/ipsec.secrets includes just the l2tp plugin's
  # secrets file and connecting fails.
  services.libreswan.enable = true;

  # Libreswan 5 defaults to `ikev1-policy=drop`, which does not merely refuse
  # IKEv1 packets but makes loading an IKEv1 connection fail outright ("failed
  # to add connection: global ikev1-policy=drop does not allow IKEv1
  # connections").  The plugin always negotiates IKEv1 (it emits ikev2=never),
  # so without this the profile is rejected before anything is sent.  "accept"
  # is required: even "reject" blocks the connection from loading.
  #
  # This goes in its own file rather than through services.libreswan.configSetup,
  # which would replace nixpkgs' default (option defaults only apply when there
  # are no definitions) and drop virtual_private=, which pluto really does use
  # to recognise NAT'ed subnets (/etc/ipsec.conf includes /etc/ipsec.d/*.conf).
  environment.etc."ipsec.d/02-vpn-fortinet.conf".text = ''
    config setup
      ikev1-policy=accept
  '';

  # pluto only reads `config setup` at startup, and the file above is not part
  # of the libreswan module's restartTriggers, so without this a rebuild would
  # leave a running pluto on the old policy.
  systemd.services.ipsec.restartTriggers = [
    config.environment.etc."ipsec.d/02-vpn-fortinet.conf".source
  ];
}
