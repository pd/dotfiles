{ buildGoModule, ... }:
buildGoModule {
  name = "filebotd";
  src = ./.;
  vendorHash = null;
  env.CGO_ENABLED = 0;
}
