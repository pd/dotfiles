{
  description = "nixen";

  outputs =
    inputs@{ nixpkgs, ... }:
    let
      lib =
        nixpkgs.lib
        // (import ./lib {
          inherit (inputs) dmerge;
          inherit lib;
        });

      specialArgs = {
        inherit inputs lib;
        inherit (lib) pd;
      };

      mkNixos =
        host: modules:
        lib.nixosSystem {
          inherit specialArgs;
          modules = [
            ./modules/core/nixos
            inputs.sops-nix.nixosModules.sops
            ./hosts/${host}
          ] ++ modules;
        };

      mkDarwin =
        host: modules:
        inputs.nix-darwin.lib.darwinSystem {
          inherit specialArgs;
          modules = [
            ./modules/core/darwin
            inputs.sops-nix.darwinModules.sops
            ./hosts/${host}
          ] ++ modules;
        };

      extraSpecialArgs = {
        inherit inputs;
        inherit (lib) pd;
      };

      mkHome =
        userAtHost: system:
        let
          extraSpecialArgs = {
            inherit inputs;
            inherit (lib) pd;
            hostname = lib.last (lib.splitString "@" userAtHost);
          };
        in
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit extraSpecialArgs;
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            ./modules/core/nixpkgs.nix
            {
              imports = [
                inputs.sops-nix.homeManagerModules.sops
                inputs.stylix.homeModules.stylix
                inputs.nixvim.homeModules.nixvim
              ];
            }
            ./homes/${userAtHost}
          ];
        };

      forEachSystem = lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    {
      formatter = forEachSystem (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);

      nixosConfigurations.desk = mkNixos "desk" [
        inputs.disko.nixosModules.disko
        inputs.nix-index-database.nixosModules.nix-index
        inputs.stylix.nixosModules.stylix
      ];

      darwinConfigurations.span = mkDarwin "span" [
        inputs.nix-index-database.darwinModules.nix-index
      ];

      nixosConfigurations.orb = mkNixos "orb" [ ];

      nixosConfigurations.donix = mkNixos "donix" [ ];
      nixosConfigurations.htpc = mkNixos "htpc" [ ];
      nixosConfigurations.pi = mkNixos "pi" [ ];

      homeConfigurations."pd@desk" = mkHome "pd@desk" "x86_64-linux";
      homeConfigurations."pd@span" = mkHome "pd@span" "x86_64-darwin";
      homeConfigurations."pd@orb" = mkHome "pd@orb" "x86_64-linux";

      # routers
      packages = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          mkRouter =
            host:
            pkgs.callPackage ./pkgs/router {
              inherit (inputs) dmerge dewclaw;
              inherit lib host pkgs;
              path = ./routers/${host};
            };
        in
        rec {
          wrt = mkRouter "wrt";
          rpt = mkRouter "rpt";
          routers = pkgs.buildEnv {
            name = "routers";
            paths = [
              wrt
              rpt
            ];
          };
        }
      );
    };

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
}
