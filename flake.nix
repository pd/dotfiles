{
  description = "nixen";

  outputs =
    inputs@{ self, nixpkgs, ... }:
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
          ]
          ++ modules;
        };

      mkDarwin =
        host: modules:
        inputs.nix-darwin.lib.darwinSystem {
          inherit specialArgs;
          modules = [
            ./modules/core/darwin
            inputs.sops-nix.darwinModules.sops
            inputs.nix-homebrew.darwinModules.nix-homebrew
            {
              nix-homebrew = {
                enable = true;
                user = "pd";
                taps = {
                  "homebrew/homebrew-core" = inputs.homebrew-core;
                  "homebrew/homebrew-cask" = inputs.homebrew-cask;
                };
                autoMigrate = true;
                mutableTaps = false;
              };
            }
            (
              { config, ... }:
              {
                homebrew.taps = builtins.attrNames config.nix-homebrew.taps;
              }
            )
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
            ./modules/core/nixpkgs.nix
            {
              imports = [
                inputs.sops-nix.homeManagerModules.sops
                inputs.stylix.homeModules.stylix
                inputs.nixvim.homeModules.nixvim
              ];

              nixpkgs.overlays =
                if system == "x86_64-darwin" || system == "aarch64-darwin" then
                  # On darwin, use the emacs from nix-darwin-emacs and packages from
                  # emacs-overlay.
                  [
                    inputs.nix-darwin-emacs.overlays.emacs
                    inputs.emacs-overlay.overlays.package
                  ]
                else
                  # On linux, use emacs-overlay for the packages and FromUsePackage
                  # helper, but we run an emacs from nixpkgs-unstable.
                  [
                    inputs.emacs-overlay.overlays.default
                  ];
            }
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

      darwinConfigurations.armspan = mkDarwin "armspan" [
        inputs.nix-index-database.darwinModules.nix-index
        ./modules/fonts
      ];

      nixosConfigurations.htpc = mkNixos "htpc" [
        inputs.quadlet-nix.nixosModules.quadlet
      ];

      nixosConfigurations.orb = mkNixos "orb" [ ];
      nixosConfigurations.donix = mkNixos "donix" [ ];
      nixosConfigurations.pi = mkNixos "pi" [ ];

      homeConfigurations."pd@desk" = mkHome "pd@desk" "x86_64-linux";
      homeConfigurations."pd@armspan" = mkHome "pd@armspan" "aarch64-darwin";
      homeConfigurations."pd@htpc" = mkHome "pd@htpc" "x86_64-linux";
      homeConfigurations."pd@orb" = mkHome "pd@orb" "aarch64-linux";

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
          stevenblack-blocklist = inputs.stevenblack-blocklist;
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
          default = pkgs.mkShell ({
            buildInputs = [
              # core
              pkgs.direnv
              pkgs.git
              pkgs.home-manager
              pkgs.just
              pkgs.nix-direnv
              pkgs.opentofu
              pkgs.sops

              # dev
              pkgs.go
              pkgs.rustc
              pkgs.rustfmt
              pkgs.cargo
              pkgs.clippy
            ];

            # Prevents devshell from polluting cflags or maybe gcc
            # invocation or something that injects `_FORTIFY_SOURCE`
            # which then breaks dlv
            hardeningDisable = [ "fortify" ];
          });
        }
      );
    };

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
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
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin-emacs = {
      url = "github:nix-giant/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quadlet-nix = {
      url = "github:SEIAROTg/quadlet-nix";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew = {
      url = "github:zhaofengli/nix-homebrew";
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
      url = "github:StevenBlack/hosts";
      flake = false;
    };
  };
}
