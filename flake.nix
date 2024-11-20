{
  description = "nixen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    private = {
      # TODO would prefer to just keep using a submodule as before
      # https://github.com/NixOS/nix/pull/7862
      url = "git+ssh://git@github.com/pd/dotfiles.private.git";
      flake = false;
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;

      specialArgs = {
        inherit inputs;
        net = import ./modules/net.nix { inherit lib; };
      };

      overlaysFor = system: {
        nixpkgs = {
          config.allowUnfree = true;
          overlays = [
            (final: prev: {
              unstable = import inputs.nixpkgs-unstable {
                inherit system;
                config.allowUnfree = true;
              };
            })
          ];
        };
      };

      homeManagerModules = {
        home-manager.sharedModules = [
          inputs.sops-nix.homeManagerModules.sops
          inputs.nixvim.homeManagerModules.nixvim
        ];
      };

      eachSystem = lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
      ];
    in
    {
      formatter = eachSystem (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);

      # nixos-rebuild switch --flake .#desk
      nixosConfigurations.desk = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          inputs.disko.nixosModules.disko
          inputs.sops-nix.nixosModules.sops
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          homeManagerModules
          ./hosts/desk
        ];
      };

      # nixos-rebuild switch --flake .#donix --target-host donix --build-host donix --use-remote-sudo
      nixosConfigurations.donix = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          "${inputs.nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          inputs.sops-nix.nixosModules.sops
          ./hosts/donix
        ];
      };

      # nixos-rebuild switch --flake .#htpc --target-host htpc --build-host htpc --use-remote-sudo
      nixosConfigurations.htpc = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          inputs.sops-nix.nixosModules.sops
          ./hosts/htpc
        ];
      };

      # building the SD, from desk with aarch64 emu:
      # nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#pi --system aarch64-linux -o ./pi.sd
      nixosConfigurations.pi = lib.nixosSystem rec {
        inherit specialArgs;
        system = "aarch64-linux";
        modules = [
          (overlaysFor system)
          inputs.sops-nix.nixosModules.sops
          ./hosts/pi
        ];
      };

      # only from span:
      # darwin-rebuild switch --flake .#span
      darwinConfigurations.span = inputs.nix-darwin.lib.darwinSystem rec {
        inherit specialArgs;
        system = "x86_64-darwin";
        modules = [
          (overlaysFor system)
          inputs.home-manager.darwinModules.home-manager
          homeManagerModules
          ./hosts/span
        ];
      };
    };
}
