{ buildGoModule, ... }:
buildGoModule {
  name = "npd";
  src = ./.;
  vendorHash = null;
  env.CGO_ENABLED = 0;
}
