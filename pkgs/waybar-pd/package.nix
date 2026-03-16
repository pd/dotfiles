{ lib, rustPlatform, makeWrapper, sway-audio-idle-inhibit }:

rustPlatform.buildRustPackage {
  pname = "waybar-pd";
  version = "0.2.0";
  src = ./.;
  cargoHash = "sha256-nD76RgphGzgm+uIlPzmw/F5CC3rZJOijAa1lZja2sIU=";

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/waybar-pd \
      --prefix PATH : ${lib.makeBinPath [ sway-audio-idle-inhibit ]}
  '';

  meta = {
    mainProgram = "waybar-pd";
    platforms = lib.platforms.linux;
  };
}
