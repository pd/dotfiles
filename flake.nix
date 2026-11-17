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
            inputs.sops-nix.nixosModules.sops
            inputs.agenix.nixosModules.default
            ./modules/core/nixos
            ./hosts/${host}
          ]
          ++ modules;
        };

      mkDarwin =
        host: modules:
        inputs.nix-darwin.lib.darwinSystem {
          inherit specialArgs;
          modules = [
            inputs.sops-nix.darwinModules.sops
            inputs.agenix.darwinModules.default
            inputs.nix-homebrew.darwinModules.nix-homebrew
            inputs.nix-index-database.darwinModules.nix-index
            ./modules/core/darwin
            ./hosts/${host}
          ]
          ++ modules;
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
            inputs.sops-nix.homeManagerModules.sops
            inputs.agenix.homeManagerModules.default
            inputs.stylix.homeModules.stylix
            inputs.nixvim.homeModules.nixvim
            ./modules/core/nixpkgs.nix
            (import ./homes/pd/overlays.nix { inherit inputs; })
            ./homes/${userAtHost}
          ];
        };

      forEachSystem =
        fn:
        lib.genAttrs
          [
            "x86_64-linux"
            "aarch64-linux"
            "x86_64-darwin"
            "aarch64-darwin"
          ]
          (
            system:
            fn {
              inherit system;
              pkgs = nixpkgs.legacyPackages.${system};
              unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
            }
          );
    in
    {
      formatter = forEachSystem ({ unstable, ... }: unstable.nixfmt);

      nixosConfigurations.desk = mkNixos "desk" [
        inputs.disko.nixosModules.disko
        inputs.nix-index-database.nixosModules.nix-index
        inputs.stylix.nixosModules.stylix
        ./modules/fonts
      ];

      nixosConfigurations.htpc = mkNixos "htpc" [
        inputs.quadlet-nix.nixosModules.quadlet
      ];

      darwinConfigurations.armspan = mkDarwin "armspan" [
        ./modules/fonts
      ];

      darwinConfigurations.mini = mkDarwin "mini" [ ];

      nixosConfigurations.donix = mkNixos "donix" [ ];
      nixosConfigurations.pi = mkNixos "pi" [ ];

      homeConfigurations."pd@desk" = mkHome "pd@desk" "x86_64-linux";
      homeConfigurations."pd@htpc" = mkHome "pd@htpc" "x86_64-linux";

      homeConfigurations."pd@armspan" = mkHome "pd@armspan" "aarch64-darwin";
      homeConfigurations."pd@mini" = mkHome "pd@mini" "aarch64-darwin";

      packages = forEachSystem (
        { pkgs, unstable, ... }:
        let
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
          inherit (inputs) stevenblack-blocklist hagezi-lgwebos;
        })
      );

      devShells = forEachSystem (
        {
          system,
          pkgs,
          unstable,
          ...
        }:
        {
          default = pkgs.mkShell {
            buildInputs = [
              # core
              pkgs.direnv
              pkgs.git
              pkgs.gnused
              pkgs.home-manager
              pkgs.just
              pkgs.nix-direnv
              pkgs.opentofu

              # secrets
              inputs.agenix.packages.${system}.agenix
              pkgs.sops

              # dev
              pkgs.go
              pkgs.rustc
              pkgs.rustfmt
              pkgs.cargo
              pkgs.clippy

              # nix fmt/lint
              unstable.nixfmt
              pkgs.deadnix
              pkgs.statix
            ];

            # Prevents devshell from polluting cflags or maybe gcc
            # invocation or something that injects `_FORTIFY_SOURCE`
            # which then breaks dlv
            hardeningDisable = [ "fortify" ];
          };
        }
      );
    };

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew = {
      url = "github:zhaofengli/nix-homebrew";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-26.05";
    };

    quadlet-nix = {
      url = "github:SEIAROTg/quadlet-nix";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };

    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };

    stevenblack-blocklist = {
      url = "file+https://raw.githubusercontent.com/StevenBlack/hosts/35c74714ad86b36806b3f40f3c36abf8b2063cb5/hosts";
      flake = false;
    };

    hagezi-lgwebos = {
      url = "file+https://raw.githubusercontent.com/hagezi/dns-blocklists/c5b86b28c193e4bfbd79c36d652e626f61d143fb/dnsmasq/native.lgwebos.txt";
      flake = false;
    };
  };
}
