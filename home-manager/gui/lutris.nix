{ pkgs, ... }:
let
  inherit (pkgs) proton-ge-bin;
in
{
  programs.lutris = {
    enable = true;
    package = pkgs.lutris.override {
      # Intercept buildFHSEnv to modify target packages
      buildFHSEnv =
        args:
        pkgs.buildFHSEnv (
          args
          // {
            multiPkgs =
              envPkgs:
              let
                # Fetch original package list
                originalPkgs = args.multiPkgs envPkgs;

                # Disable tests for openldap
                customLdap = envPkgs.openldap.overrideAttrs (_: {
                  doCheck = false;
                });
              in
              # Replace broken openldap with the custom one
              builtins.filter (p: (p.pname or "") != "openldap") originalPkgs ++ [ customLdap ];
          }
        );
    };
    defaultWinePackage = proton-ge-bin;
    protonPackages = [ proton-ge-bin ];
    winePackages = [ pkgs.wineWow64Packages.full ];
  };
}
