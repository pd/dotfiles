{
  description = "nixen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # dotfiles = {
    #   url = "github:pd/dotfiles";
    #   flake = false;
    # };
  };

  outputs = inputs@{
    self,
    nixpkgs,
    nixos-generators,
    sops-nix,
    home-manager,
    ...
  }: let
    system = "x86_64-linux";
  in {
    nixosConfigurations = {
      donix = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/donix
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
        ];
        # specialArgs = { dotfiles = inputs.dotfiles; };
      };

      desk = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/desk
          sops-nix.nixosModules.sops
        ];
      };
    };

    images = {
      donix = nixos-generators.nixosGenerate {
        system = "x86_64-linux";
        format = "do";
        modules = [ ./hosts/donix ];
      };
    };
  };
}
