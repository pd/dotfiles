{ lib, rustPlatform, makeWrapper, sway-audio-idle-inhibit }:

rustPlatform.buildRustPackage {
  pname = "waybar-pd";
  version = "0.2.0";
  src = ./.;
  cargoHash = "sha256-QTNWnVopjP5M+UwD/81N2tmR3EOhUtWC+/ZPhAy2GHY=";

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
