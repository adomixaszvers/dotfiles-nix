{ pkgs, config, ... }:
{
  home = {
    packages = [ pkgs.krb5 ];
    file.".k5identity".text = ''
      adomas.jatuzis@X.INSOFT.LT realm=X.INSOFT.LT
    '';
    sessionVariables.KRB5_CLIENT_KTNAME = "${config.xdg.stateHome}/insoft.keytab";
  };
  programs.firefox = {
    policies = {
      Authentication = {
        SPNEGO = [ ".insoft.lt" ];
        NTLM = [ ".insoft.lt" ];
      };
    };
  };
}
