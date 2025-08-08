{ fetchFromGitHub, buildGoModule }:
buildGoModule {
  name = "rtorrent-exporter";
  version = "";

  src = fetchFromGitHub {
    owner = "pd";
    repo = "rtorrent-exporter";
    rev = "size-metric";
    hash = "sha256-KT4RLWxtrBHWhwwt35E7UO3c7r5MZ7oVSdMSo6VMiNo=";
  };

  vendorHash = "sha256-xud2r1dDyYv9ImmnhF6sPGtyfpPCPKRIz31Vb1dLq10=";
  env.CGO_ENABLED = 0;
}
