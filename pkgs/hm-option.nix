{
  nix,
  hostname,
  nixos-option,
  writeShellScriptBin,
  lib,
  git,
}:
writeShellScriptBin "hm-option" ''
  PATH=${
    lib.makeBinPath [
      nix
      nixos-option
      hostname
      git
    ]
  }
  hasConfig=$(nix-instantiate --expr "let flake = import $HOME/.config/nixpkgs; in flake.nixosConfigurations ? $(hostname)" --eval)
  if [ "$hasConfig" = true ]; then
    exec nixos-option --config_expr "let flake = import $HOME/.config/nixpkgs; in flake.nixosConfigurations.$(hostname).config" --options_expr "let flake = import $HOME/.config/nixpkgs; in flake.nixosConfigurations.$(hostname).options" "$@"

  fi
''
