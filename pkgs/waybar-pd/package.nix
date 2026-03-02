{
  lib,
  stdenv,
  callPackage,

  systemdLibs,
  pkg-config,
  wayland,
  wayland-protocols,
  wayland-scanner,
  zig,
}:

stdenv.mkDerivation (final: {
  pname = "waybar-pd";
  version = "0.1.0";

  src = ./.;
  outputs = [ "out" ];

  buildInputs = [ ];

  nativeBuildInputs = [
    zig.hook
    pkg-config
    systemdLibs
    wayland
    wayland-protocols
    wayland-scanner
  ];

  # Generate build.zig.zon.nix with:
  #     nix run github:Cloudef/zig2nix#zon2nix
  postConfigure = ''
    ln -s ${callPackage ./build.zig.zon.nix { inherit zig; }} $ZIG_GLOBAL_CACHE_DIR/p
  '';

  meta = {
    mainProgram = "waybar-pd";
    platforms = lib.platforms.linux;
  };
})
