{ pkgs, inputs, lib, ... }: {
  imports = [ ./cli ];
  home.packages = let
    # see https://www.reddit.com/r/SteamDeck/comments/yxpmlq/nix_homemanager_on_steam_deck/
    wrapProgram = { wrapper, pkg, extraWrapperArgs ? "" }:
      pkgs.runCommand "${pkg.pname or pkg.name}-wrapped" {
        pname = pkg.pname or pkg.name;
        version = pkg.version or null;
        passthru = pkg.passthru or { };

        buildInputs = [ pkgs.makeWrapper ];
      } ''
        mkdir -p $out/bin
        # link everything but bin
        shopt -s extglob
        ln -s ${pkg}/!(bin) $out
        for exe in ${pkg}/bin/*; do
          target="$out/bin/$(basename "$exe")"
          if [[ -x "$exe" ]]; then
            makeWrapper "${wrapper}" "$target" \
              --inherit-argv0 \
              --add-flags "$exe" ${extraWrapperArgs}
          else
            ln -s "$exe" "$target"
          fi
        done
      '';
    inherit (import inputs.nixGL {
      inherit pkgs;
      enable32bits = false;
      enableIntelX86Extensions = false;
    })
      nixGLMesa;
  in with pkgs; [
    # firefox # does not detect amd gpu
    (wrapProgram {
      wrapper = "${nixGLMesa}/bin/nixGLMesa";
      pkg = firefox;
    })
    keepassxc
    xsel
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
    SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh";
  };
  manual.html.enable = true;
  programs.gpg.enable = lib.mkForce false;
  services = {
    gpg-agent.enable = false;
    syncthing = {
      enable = true;
      # tray.enable = true;
    };
  };
  xdg = { enable = true; };
  targets.genericLinux.enable = true;
}
