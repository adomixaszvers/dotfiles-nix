# AGENTS.md

## Project

Personal NixOS + home-manager dotfiles, packaged as a single Nix flake ("My dotfiles").
Flake description: `My dotfiles`. Systems: `x86_64-linux` only. Built with `flake-parts`.
nixpkgs input: `github:NixOS/nixpkgs/nixos-unstable` (`nixos-unstable` aliases `nixpkgs`).

- Flake outputs are wired in `flake.nix`, which imports:
  `./wrappers`, `./modules/nixbotJobs.nix`, `./pkgs`, `./devshells.nix`, `./nixos`, `./home-manager`, `./nixbotJobs.nix`.
- `pkgs` and `unstable` are built as `import nixpkgs { inherit system; config = import ./config.nix; }` (per system).
- Non-flake entry points: `default.nix` / `shell.nix` / `flakeCompat.nix` (flake-compat shims); `repl.nix` for `nix repl`.
- `.envrc` runs `use flake` (direnv); `.editorconfig` has `root = true`.

## Repository layout

- `flake.nix` — flake inputs/outputs; treefmt (`nixfmt`, `statix`, `deadnix`, `keep-sorted`, `stylua`, `shellcheck`).
- `home-manager/` — home-manager configuration.
  - `home-manager/default.nix` — `flake.homeConfigurations`: `work`, `asus-laptop`, `t14`, `foreign`, `deck`
    plus aliases `adomas@adomo-nixos`, `adomas@adomo-t14`, `adomas@arch-vm`, `adomas@adomas-jatuzis-nixos`, `deck@steamdeck`;
    `home.stateVersion = "25.05"`; `extraSpecialArgs` = `inputs`, `unstable`, `system`, `myPkgs` (= `self'.packages`);
    per-config files (`t14.nix`, `steamdeck.nix`, `work/`, ...); `programs.home-manager.enable = true`.
  - `home-manager/common.nix` — shared imports (`./cli`, `./gui`, `./wm/common.nix`); `systemd.user.startServices = "sd-switch"`.
  - `home-manager/modules/` — own home-manager modules; `home-manager/cli`, `home-manager/gui`, `home-manager/wm`, `home-manager/work`.
- `nixos/` — `flake.nixosConfigurations`: `adomo-nixos`, `adomas-jatuzis-nixos`, `adomo-pc-nixos`, `adomo-t14`, `beelink`
  (`pi` / raspberrypi entry commented out); machine configs under `nixos/<host>/` incl. `hardware-configuration.nix`
  (excluded from statix); `nixos/keys`; `nixos/common-secrets`; `specialArgs = { inherit inputs; }`.
- `pkgs/` — custom packages via `pkgs.callPackage`, exposed under `flake.packages.x86_64-linux` (`myPkgs`);
  also per-system `he` and `hm-repl`; `pkgs/default.nix` lists the package set; unfree handling in `config.nix`.
- `wrappers/` — `nix-wrapper-modules`; `wrappers.neovim` / `wrappers.neovim-nix`; `control_type = "exclude"`.
- `modules/` — custom flake-parts modules (`modules/nixbotJobs.nix` defines transposed `nixbotJobs` option).
- `devshells.nix` — `devShells.default` (`mkShellNoCC` with home-manager, sops, age, `treefmt` wrapper);
  `devShells.x86_64-linux.xmonad` / `awesomewm`.
- `nixbotJobs.nix` + `nixbot.toml` — CI job set under attribute `nixbotJobs` (nixos machines, packages, checks, devShells);
  `nixbot.toml`: `attribute = "nixbotJobs"`, `flake_dir = "."`, `lock_file = "./flake.lock"`.
- `config.nix` — nixpkgs `config`: `allowUnfreePredicate` allowlist (`# keep-sorted` sorted), `permittedInsecurePackages`,
  `joypixels.acceptLicense`, `vim.ftNix = false`.
- `profiles/`, `installers/`, `hm-secrets/`, `nixos/keys`, `.sops.yaml` — secrets / profiles / installers (see Secrets).
- `flake.lock`, `repl.nix`, `flakeCompat.nix` — misc.
- `.stylua.toml`, `.editorconfig`, `.dir-locals.el`, `.gitignore`, `.envrc`, `.sops.yaml`.

## Common commands

Run inside the dev shell: `nix develop` (or `direnv allow` with the `use flake` `.envrc`).

```sh
nix fmt                 # format whole repo via treefmt (nixfmt, statix, deadnix, keep-sorted, stylua, shellcheck)
treefmt                 # same, using the treefmt in the dev shell
nix fmt -- --fail-on-change # fail if formatting changes anything; `nix flake check` runs checks.x86_64-linux.treefmt
nix flake check         # evaluate + build flake checks; what CI runs
nix-instantiate --parse # syntax check a .nix file (CI parses every .nix)
nix build .#<package>   # build a package from `pkgs/` / wrappers outputs
nix build .#nixosConfigurations.<host>.config.system.build.toplevel
nix develop             # dev shell
nix repl                # repo flake attrset; `nix repl --file repl.nix` adds pkgs/unstable/lib
nix flake update        # update flake.lock (CI does this weekly via Forgejo workflow, opens a PR)
```

Apply configurations (names from `home-manager/default.nix` / `nixos/default.nix`).
Use `nh` (nix-helper) instead of `home-manager switch` / `nixos-rebuild switch`:

```sh
nh home switch .#t14            # home-manager; or .#asus-laptop / .#work / .#foreign / .#deck / .#adomas@adomo-t14 ...
nh os switch .#adomo-t14        # NixOS; or .#adomo-nixos / .#adomas-jatuzis-nixos / .#adomo-pc-nixos / .#beelink
nh os test .#<host>             # activate without making boot default; `nh os build` / `nh home build` to just build
```

nh handles privilege elevation itself (`-e`/`--elevation-strategy`, auto doas/sudo/run0/pkexec); no manual `sudo`.
`nh home repl` / `nh os repl` for repls. `nh` is on PATH, not in `nix develop`.

`.dir-locals.el` sets `projectile-project-compilation-cmd` to `hm-switch` (editor integration; not provided by this repo).

Secrets tooling in the dev shell: `sops`, `age`. `.sops.yaml` defines PGP/age recipients per `path_regex`.

## Conventions

**Nix**
- Format with `nixfmt`; 2-space indent; LF; final newline; trim trailing whitespace; UTF-8 (`.editorconfig`).
- Must be `statix`-clean and `deadnix`-clean; `hardware-configuration.nix` files are exempt from statix.
- Keep lists sorted; wrap sortable regions in keep-sorted marker comments (see `config.nix` for the marker syntax).
- Unfree packages go in `config.nix` `allowUnfreePredicate` allowlist (keep it `keep-sorted` sorted); insecure ones in `permittedInsecurePackages`.
- Custom packages: `pkgs.callPackage ./<name>.nix { }`; register in `pkgs/default.nix`.
- New NixOS modules under `nixos/`; new home-manager modules under `home-manager/modules/`; shared WM stuff under `home-manager/wm/`.
- `specialArgs`/`extraSpecialArgs` carry `inputs`; `myPkgs`, `unstable`, `system` in home-manager; don't re-import nixpkgs ad hoc — reuse `pkgs`/`unstable`.

**Lua** (`wrappers/neovim`, `home-manager/wm/awesome`, ...): `stylua`; `.stylua.toml` / treefmt settings —
column_width 100, Unix endings, spaces 2, `AutoPreferSingle` quotes, `call_parentheses = "Input"`; `.stylua.toml` at root.

**Shell**: `shellcheck`-clean; 2-space indent via `.editorconfig`; keep scripts `#!/usr/bin/env bash` where applicable.

**EditorConfig**: don't add entries for subfolders to root `.editorconfig`; create `<subfolder>/.editorconfig` instead;
markdown indent 2, no hard line breaks.

**Search**: use `rg` (ripgrep) for grepping file contents; it respects `.gitignore`; avoid `grep -r`.

**VCS**: use `jj` (Jujutsu) for version control; repo is colocated (`.jj/` + `.git/`), `jj` auto-exports to Git;
prefer `jj` (`jj status`, `jj diff`, `jj commit -m ...`) over raw `git`. `.gitignore` `result*`, `.direnv`, `.idea`, `.luacheckcache`;
`flake.lock` is committed; don't commit `result*` symlinks.

## Secrets

- Secrets are `sops-nix`-encrypted; recipients in `.sops.yaml` (PGP `C120E76F...`, age keys per host), `sops` + `age` in dev shell.
- `hm-secrets/keys.yaml`, `nixos/common-secrets/`, `nixos/keys`, `nixos/**/secrets` — encrypted; edit with `sops`, never print/leak key material.
- Never commit decrypted secrets, `.age` keys, or `result*`.

## CI

- `.github/workflows/flake-check.yml`: `nix-instantiate --parse` every `.nix`, then `nix flake check`
  (extra substituters: `nix-community`, `adomixaszvers`, `cache.nixos-cuda` cachix).
- `.forgejo/workflows/flake-updates.yaml`: weekly `nix flake update --commit-lock-file`, opens PR (`create-pr.sh`).
- nixbot builds `nixbotJobs` (`nixbot.toml`); `.github/dependabot.yaml`.
