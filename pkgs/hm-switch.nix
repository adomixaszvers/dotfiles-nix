{ writeShellScriptBin, home-manager }:
writeShellScriptBin "hm-switch" ''
  config="git+file://$HOME/.config/nixpkgs"

  if [ -x $XDG_DATA_HOME/bin/detect-hm-config ]; then
    detected="$($XDG_DATA_HOME/bin/detect-hm-config)" && config="''${config}#''${detected}"
  fi

  echo "switching to $config"
  exec ${home-manager}/bin/home-manager switch --flake "$config"
''
