{
  security.krb5 = {
    enable = true;
    settings = {
      libdefaults = {
        default_realm = "X.INSOFT.LT";
        default_ccache_name = "KEYRING:persistent:%{uid}";
      };
      realms."X.INSOFT.LT".pkinit_anchors = "FILE:${./insoft-ca.crt}";
    };
  };
  programs = {
    chromium = {
      enable = true;
      extraOpts = {
        AuthServerAllowlist = "*.insoft.lt,insoft.lt";
        AuthNegotiateDelegateAllowlist = "*.insoft.lt,insoft.lt";
      };
    };
  };
}
