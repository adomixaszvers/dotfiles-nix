{
  inputs,
  ...
}:
let
  inputNames = [
    "home-manager"
    "nixos-hardware"
    "nixpkgs"
    "nixos-unstable"
    "stylix"
  ];
in
{
  xdg.configFile = builtins.listToAttrs (
    builtins.map (n: {
      name = "flakeInputs/${n}";
      value = {
        source = builtins.getAttr n inputs;
      };
    }) inputNames
  );
  nix.nixPath = builtins.map (n: "${n}=${builtins.getAttr n inputs}") inputNames;
}
