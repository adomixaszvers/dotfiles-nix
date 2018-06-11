{ lib, bundlerApp, vim }:

bundlerApp {
  pname = "vimgolf";
  gemdir = ./.;
  exes = [ "vimgolf" ];
}
