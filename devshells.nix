{ withSystem, ... }:
{
  flake.devShells.x86_64-linux = withSystem "x86_64-linux" (
    { pkgs, ... }:
    {

      xmonad = import ./home-manager/wm/xmonad/shell.nix { inherit pkgs; };
      # TODO uncomment when https://github.com/NixOS/nixpkgs/pull/522705 is merged
      # qtile = import ./home-manager/wm/qtile/shell.nix { inherit pkgs; };
      awesomewm = import ./home-manager/wm/awesome/shell.nix { inherit pkgs; };
    }
  );
  perSystem =
    {
      pkgs,
      inputs',
      config,
      ...
    }:
    {
      devShells.default = pkgs.mkShellNoCC {
        packages = [
          inputs'.home-manager.packages.home-manager
          pkgs.sops
          pkgs.age
          config.treefmt.build.wrapper
        ];
      };
    };
}
