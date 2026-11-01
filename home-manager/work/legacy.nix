{ myPkgs, ... }:
{
  home = {
    file = {
      "jdks/oraclejdk8".source = myPkgs.oraclejdk8;
    };
    packages = [
      myPkgs.eclipse-activiti
      myPkgs.sqldeveloper
    ];
    sessionVariables = {
      CS_AUTH_KEYS = "/home/adomas/HSMrsa.pub";
      CS_PKCS11_R2_CFG = "/home/adomas/cs_pkcs11_R2.cfg";
      CRYPTOSERVER = "3001@localhost";
    };
  };
}
