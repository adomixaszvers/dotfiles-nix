{
  security.krb5 = {
    enable = true;
    settings = {
      libdefaults = {
        default_realm = "X.INSOFT.LT";
        default_ccache_name = "/home/%{username}/.cache/krb5cc";
      };
      realms = {
        "X.INSOFT.LT" = {
          admin_server = "dc01.x.insoft.lt";
          kdc =
            [ "dc02.x.insoft.lt" "dcaz01.x.insoft.lt" "dcaz02.x.insoft.lt" ];
          pkinit_anchors = "FILE:${./insoft-ca.crt}";
        };
      };
    };
  };
  programs = {
    chromium = {
      enable = true;
      # homepageLocation = "https://starter.x.insoft.lt";
      extraOpts = {
        AuthServerAllowlist = "*.insoft.lt,insoft.lt";
        AuthNegotiateDelegateAllowlist = "*.insoft.lt,insoft.lt";
      };
    };
  };
}
