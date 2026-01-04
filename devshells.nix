{ withSystem, ... }:
{
  flake.devShells.x86_64-linux = withSystem "x86_64-linux" (
    { pkgs, ... }:
    {

      xmonad = import ./profiles/wm/xmonad/shell.nix { inherit pkgs; };
      qtile = import ./profiles/wm/qtile/shell.nix { inherit pkgs; };
      awesomewm = import ./profiles/wm/awesome/shell.nix { inherit pkgs; };
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
