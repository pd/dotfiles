{
  description =
    "nixen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    srvos.url = "github:nix-community/srvos";
  };

  outputs = inputs@{ self, nixpkgs, nixos-generators, ... }:
    let
      system = "x86_64-linux";
    in {

      nixosConfigurations = {
        donix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./donix.nix
            "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          ];
        };

        nuc = {
          system = "x86_64-linux";
          modules = [
            ./nuc.nix
          ];
        };
      };

      images = {
        donix = nixos-generators.nixosGenerate {
          system = "x86_64-linux";
          format = "do";
          modules = [ ./donix.nix ];
        };
      };

    };
}
