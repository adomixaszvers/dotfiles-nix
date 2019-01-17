{ pkgs, polybar, polybar-config}:
let callPackage = pkgs.callPackage;
in
{
  check-all-updates = callPackage ./check-all-updates.nix {};
  compton = callPackage ./compton.nix {};
  compton-toggle = callPackage ./compton-toggle.nix {};
  launch = callPackage ./launch.nix { inherit polybar polybar-config; };
  pavolume = callPackage ./pavolume.nix {};
  pub-ip = callPackage ./pub-ip.nix {};
  spotify1 = callPackage ./spotify1.nix {};
  tempcores = callPackage ./tempcores.nix {};
  weather = callPackage ./weather.nix {};
}

