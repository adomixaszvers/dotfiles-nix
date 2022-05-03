{ lib, stdenv, fetchgit, zig, wayland, pkg-config, river, scdoc, xwayland
, wayland-protocols, wlroots, libxkbcommon, pixman, udev, libevdev, libinput
, libX11, libGL }:

stdenv.mkDerivation rec {
  pname = "rivercarro";
  version = "0.1.3";

  src = fetchgit {
    url = "https://git.sr.ht/~novakane/rivercarro";
    rev = "v${version}";
    hash = "sha256-8CehmrErukzWa43i8n0kZDMEDCJBhd2k46IR26YC7JM=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ river zig wayland xwayland scdoc pkg-config ];

  buildInputs = [
    wayland-protocols
    wlroots
    libxkbcommon
    pixman
    udev
    libevdev
    libinput
    libX11
    libGL
  ];

  dontConfigure = true;

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    runHook preInstall
    zig build -Drelease-safe -Dcpu=baseline -Dman-pages --prefix $out install
    runHook postInstall
  '';

  /* Builder patch install dir into river to get default config
     When installFlags is removed, river becomes half broken.
     See https://github.com/ifreund/river/blob/7ffa2f4b9e7abf7d152134f555373c2b63ccfc1d/river/main.zig#L56
  */
  installFlags = [ "DESTDIR=$(out)" ];

  meta = with lib; {
    homepage = "https://git.sr.ht/~novakane/rivercarro";
    description = "A dynamic tiling wayland compositor";
    license = licenses.gpl3;
    platforms = platforms.linux;
    maintainers = [ ];
  };
}
