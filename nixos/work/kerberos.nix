{
  krb5 = {
    enable = true;
    libdefaults = {
      default_realm = "X.INSOFT.LT";
      default_ccache_name = "/home/%{username}/.cache/krb5cc";
    };
    realms."X.INSOFT.LT".pkinit_anchors = "FILE:${./insoft-ca.crt}";
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
