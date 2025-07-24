{ fetchurl, stdenv, ... }:
stdenv.mkDerivation {
  name = "rtorrent-exporter";

  src = fetchurl {
    url = "https://github.com/aauren/rtorrent-exporter/releases/download/v1.4.9/rtorrent-exporter_Linux_amd64.tar.gz";
    sha256 = "RVFyOIDe02+I7x8fz47Cvp8pKOyNIVCfpEnMi2UXNts=";
  };

  sourceRoot = ".";

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp rtorrent-exporter-*/rtorrent-exporter $out/bin
  '';
}
