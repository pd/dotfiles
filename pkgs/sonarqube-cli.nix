{
  lib,
  stdenvNoCC,
  fetchurl,
}:
let
  version = "0.8.1.798";

  sources = {
    aarch64-darwin = fetchurl {
      url = "https://binaries.sonarsource.com/Distribution/sonarqube-cli/${version}/macos/sonarqube-cli-${version}-macos-arm64.exe";
      hash = "sha256-8ohg2icOQhIm67m6hjateRwZhC2D/riI2H26+L5UEWA=";
    };
    x86_64-linux = fetchurl {
      url = "https://binaries.sonarsource.com/Distribution/sonarqube-cli/${version}/linux/sonarqube-cli-${version}-linux-x86-64.exe";
      hash = "sha256-vdvyIHk4zvgEqK+qi46sQR57xwSJQnb6o1coTcKdNgY=";
    };
  };
in
stdenvNoCC.mkDerivation {
  pname = "sonarqube-cli";
  inherit version;

  src = sources.${stdenvNoCC.hostPlatform.system};

  dontUnpack = true;

  installPhase = ''
    install -Dm755 $src $out/bin/sonar
  '';

  meta = {
    description = "Command-line interface for SonarQube";
    homepage = "https://github.com/SonarSource/sonarqube-cli";
    license = lib.licenses.lgpl3Only;
    platforms = lib.attrNames sources;
    mainProgram = "sonar";
  };
}
