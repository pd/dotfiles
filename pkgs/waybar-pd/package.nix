{
  lib,
  stdenv,
  callPackage,

  systemdLibs,
  pkg-config,
  wayland,
  wayland-protocols,
  wayland-scanner,
  zig_0_14,
}:

stdenv.mkDerivation (final: {
  pname = "waybar-pd";
  version = "0.1.0";

  src = ./.;
  outputs = [ "out" ];

  buildInputs = [ ];

  nativeBuildInputs = [
    zig_0_14.hook
    pkg-config
    systemdLibs
    wayland
    wayland-protocols
    wayland-scanner
  ];

  dontConfigure = true;
  dontUseZigCheck = true;

  # Lifted from https://github.com/NixOS/nixpkgs/blob/f9d6590c135a675078b6dc87d281b263920a6bb0/pkgs/by-name/ly/ly/package.nix#L35-L41
  # When trying to solve the issue that `zig fetch` happens at build time and fails with a DNS error:
  #     > zig build flags: -Dcpu=baseline --release=safe
  #     > /build/waybar-pd/build.zig.zon:9:20: error: unable to connect to server: TemporaryNameServerFailure
  #     >             .url = "https://codeberg.org/ifreund/zig-wayland/archive/65b01fb69148013eea882c8222a82a1d96a6241b.tar.gz",
  #     >                    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  # Generate build.zig.zon.nix with:
  #     nix run github:Cloudef/zig2nix#zon2nix
  postPatch = ''
    ln -s ${callPackage ./build.zig.zon.nix { zig = zig_0_14; }} $ZIG_GLOBAL_CACHE_DIR/p
  '';

  meta = {
    mainProgram = [ "waybar-pd" ];
    platforms = lib.platforms.linux;
  };
})
