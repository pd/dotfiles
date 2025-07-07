{
  description = "nixen";

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    private = {
      url = ./private;
      flake = false;
    };

    dewclaw = {
      url = "github:MakiseKurisu/dewclaw";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dmerge = {
      url = "github:divnix/dmerge";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;

      net = import ./modules/net.nix { inherit lib; };
      specialArgs = {
        inherit inputs net;
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

      nixosConfigurations.desk = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          inputs.disko.nixosModules.disko
          inputs.sops-nix.nixosModules.sops
          inputs.home-manager.nixosModules.home-manager
          inputs.nix-index-database.nixosModules.nix-index
          inputs.stylix.nixosModules.stylix
          homeManagerModules
          ./hosts/desk
        ];
      };

      nixosConfigurations.donix = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          inputs.sops-nix.nixosModules.sops
          ./hosts/donix
        ];
      };

      nixosConfigurations.htpc = lib.nixosSystem rec {
        inherit specialArgs;
        system = "x86_64-linux";
        modules = [
          (overlaysFor system)
          inputs.sops-nix.nixosModules.sops
          ./hosts/htpc
        ];
      };

      nixosConfigurations.pi = lib.nixosSystem rec {
        inherit specialArgs;
        system = "aarch64-linux";
        modules = [
          (overlaysFor system)
          inputs.sops-nix.nixosModules.sops
          ./hosts/pi
        ];
      };

      darwinConfigurations.span = inputs.nix-darwin.lib.darwinSystem rec {
        inherit specialArgs;
        system = "x86_64-darwin";
        modules = [
          (overlaysFor system)
          inputs.home-manager.darwinModules.home-manager
          inputs.nix-index-database.darwinModules.nix-index
          homeManagerModules
          ./hosts/span
        ];
      };

      # routers
      packages = eachSystem (
        system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          deploy = pkgs.callPackage inputs.dewclaw {
            inherit pkgs;
            configuration = import ./routers {
              inherit (inputs) dmerge;
              inherit net pkgs;
            };
          };

          configs = pkgs.runCommand "extract-configs" { } ''
            mkdir $out
            for d in ${deploy}/bin/deploy-*; do
              host="$(basename $d | sed 's/^deploy-//')"
              config="$(cat ${deploy}/bin/deploy-$host | grep 'cp.*no-preserve' | awk '{print $3}')"
              install -Dm600 $config $out/$host.uci
            done
          '';
        in
        {
          routers = pkgs.symlinkJoin {
            name = "routers";
            paths = [
              deploy
              configs
            ];
          };
        }
      );
    };
}
