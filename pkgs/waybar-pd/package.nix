{ lib, rustPlatform }:

rustPlatform.buildRustPackage {
  pname = "waybar-pd";
  version = "0.2.0";
  src = ./.;
  cargoHash = "sha256-cUdjFz7crL+yzwmtvomXQtFWuO3zwIZZbewGPiW6aVg=";

  meta = {
    mainProgram = "waybar-pd";
    platforms = lib.platforms.linux;
  };
}
