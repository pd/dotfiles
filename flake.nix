{
  description = "nixen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
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
    inputs@{ self, ... }:
    rec {
      formatter.x86_64-linux = inputs.nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;

      nixpkgs.overlays = [ (import self.inputs.emacs-overlay) ];

      net = import ./modules/net.nix { lib = inputs.nixpkgs.lib; };

      nixosConfigurations = {
        # nixos-rebuild switch --flake .#desk
        desk = inputs.nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            inherit net;
            pkgs-unstable = import inputs.nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
          };
          modules = [
            ./hosts/desk
            inputs.home-manager.nixosModules.home-manager
            inputs.sops-nix.nixosModules.sops
            inputs.stylix.nixosModules.stylix
          ];
        };

        # nixos-rebuild switch --flake .#donix --target-host donix --build-host donix --use-remote-sudo
        donix = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit net;
          };
          modules = [
            ./hosts/donix
            inputs.sops-nix.nixosModules.sops
            "${inputs.nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          ];
        };

        # nixos-rebuild switch --flake .#htpc --target-host htpc --build-host htpc --use-remote-sudo
        htpc = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit net;
          };
          modules = [
            ./hosts/htpc
            inputs.sops-nix.nixosModules.sops
          ];
        };

        # building the SD, from desk with aarch64 emu:
        # nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#pi --system aarch64-linux -o ./pi.sd
        pi = inputs.nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = {
            inherit net;
          };
          modules = [
            ./hosts/pi
            inputs.sops-nix.nixosModules.sops
          ];
        };
      };

      darwinConfigurations."span" = inputs.nix-darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          inputs.home-manager.darwinModules.home-manager
          ./hosts/span
        ];
      };

      images = {
        donix = inputs.nixos-generators.nixosGenerate {
          system = "x86_64-linux";
          format = "do";
          modules = [ ./hosts/donix ];
        };
      };
    };
}
