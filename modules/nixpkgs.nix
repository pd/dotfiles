{ inputs, ... }:
{
  nixpkgs = {
    config.allowUnfree = true;
    overlays = builtins.attrValues (import ../overlays { inherit inputs; });
  };
}
