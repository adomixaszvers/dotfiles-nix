{ fetchurl, appimageTools }:
appimageTools.wrapType1 {
  name = "sky";
  src = fetchurl {
    url = "https://www.tel.red/linux/sky-2.1.7520-1-x86_64.AppImage";
    sha256 = "1ah8a287hwhij1fqkbhni668qlck5yslsrvgcd1rhwmnrrk35lp4";
  };
  extraPkgs = pkgs: with pkgs; [ pulseaudio ];
}
