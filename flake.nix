{
  description = "nixen";

  outputs =
    inputs@{ nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;

      libx = import ./lib {
        inherit lib;
        inherit (inputs) dmerge;
      };

      specialArgs = {
        inherit inputs lib libx;
        inherit (libx) pd;
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

      mkHome =
        userAtHost: system:
        inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit inputs;
            inherit (libx) pd;
            hostname = lib.last (lib.splitString "@" userAtHost);
          };
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
        ./modules/fonts
      ];

      darwinConfigurations.span = mkDarwin "span" [
        inputs.nix-index-database.darwinModules.nix-index
        ./modules/fonts
      ];

      nixosConfigurations.orb = mkNixos "orb" [ ];
      nixosConfigurations.donix = mkNixos "donix" [ ];
      nixosConfigurations.htpc = mkNixos "htpc" [ ];
      nixosConfigurations.pi = mkNixos "pi" [ ];

      homeConfigurations."pd@desk" = mkHome "pd@desk" "x86_64-linux";
      homeConfigurations."pd@span" = mkHome "pd@span" "x86_64-darwin";
      homeConfigurations."pd@htpc" = mkHome "pd@htpc" "x86_64-linux";
      homeConfigurations."pd@orb" = mkHome "pd@orb" "x86_64-linux";

      packages = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
          mkRouter =
            host:
            pkgs.callPackage ./pkgs/router {
              inherit (inputs) dmerge dewclaw;
              inherit (libx) pd uci;
              inherit
                lib
                host
                pkgs
                ;
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
        // (import ./pkgs {
          inherit pkgs unstable;
        })
      );

      devShells = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                direnv
                home-manager
                just
                neovim
                nix-direnv
              ]
              ++ (with unstable; [
                git
                opentofu
                sops
              ]);
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
