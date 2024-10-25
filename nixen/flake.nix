{
  description = "nixen";

  # gh = repo: { url = "github:${repo}"; };
  # ghFollows = repo: (gh repo) // { inputs.nixpkgs.follows = "nixpkgs"; };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

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

  # TODO modular + home-manager as both integrated and distinct output
  # https://github.com/nmasur/dotfiles/blob/a6e4b3130d6f86303d9f80fdedbd38094d8427ac/flake.nix#L286-L307
  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      nixos-generators,
      home-manager,
      sops-nix,
      stylix,
      ...
    }:
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;

      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
      ];

      nixosConfigurations = {
        # nixos-rebuild switch --flake .#desk
        desk = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            pkgs-unstable = import nixpkgs-unstable {
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
        donix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/donix
            inputs.sops-nix.nixosModules.sops
            "${inputs.nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          ];
        };

        # nixos-rebuild switch --flake .#htpc --target-host htpc --build-host htpc --use-remote-sudo
        htpc = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/htpc
            inputs.sops-nix.nixosModules.sops
          ];
        };

        # deploying changes from desk via qemu:
        # nixos-rebuild test --flake .#pi --target-host pi --use-remote-sudo --fast
        #
        # why --fast is critical, I don't know, but it is
        #
        # building the SD, from desk with aarch64 emu:
        # nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#pi --system aarch64-linux -o ./pi.sd
        pi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            {
              nixpkgs.buildPlatform = "x86_64-linux";
              nixpkgs.hostPlatform = "aarch64-linux";
            }
            ./hosts/pi
            inputs.sops-nix.nixosModules.sops
          ];
        };
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
