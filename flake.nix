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
    inputs:
    let
      net = import ./modules/net.nix { lib = inputs.nixpkgs.lib; };

      specialArgs = {
        inherit net inputs;
      };

      withOverlays = system: {
        nixpkgs = {
          config.allowUnfree = true;
          overlays = [
            (final: prev: {
              unstable = import inputs.nixpkgs-unstable {
                system = system;
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
    in
    {
      formatter.x86_64-linux = inputs.nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;

      nixosConfigurations = {
        # nixos-rebuild switch --flake .#desk
        desk = inputs.nixpkgs.lib.nixosSystem rec {
          inherit specialArgs;
          system = "x86_64-linux";
          modules = [
            (withOverlays system)
            ./hosts/desk
            inputs.home-manager.nixosModules.home-manager
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
            inputs.stylix.nixosModules.stylix
            homeManagerModules
          ];
        };

        # nixos-rebuild switch --flake .#donix --target-host donix --build-host donix --use-remote-sudo
        donix = inputs.nixpkgs.lib.nixosSystem rec {
          inherit specialArgs;
          system = "x86_64-linux";
          modules = [
            (withOverlays system)
            ./hosts/donix
            inputs.sops-nix.nixosModules.sops
            "${inputs.nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          ];
        };

        # nixos-rebuild switch --flake .#htpc --target-host htpc --build-host htpc --use-remote-sudo
        htpc = inputs.nixpkgs.lib.nixosSystem rec {
          inherit specialArgs;
          system = "x86_64-linux";
          modules = [
            (withOverlays system)
            ./hosts/htpc
            inputs.sops-nix.nixosModules.sops
          ];
        };

        # building the SD, from desk with aarch64 emu:
        # nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#pi --system aarch64-linux -o ./pi.sd
        pi = inputs.nixpkgs.lib.nixosSystem rec {
          inherit specialArgs;
          system = "aarch64-linux";
          modules = [
            (withOverlays system)
            ./hosts/pi
            inputs.sops-nix.nixosModules.sops
          ];
        };
      };

      darwinConfigurations."span" = inputs.nix-darwin.lib.darwinSystem rec {
        inherit specialArgs;
        system = "x86_64-darwin";
        modules = [
          (withOverlays system)
          inputs.home-manager.darwinModules.home-manager
          homeManagerModules
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
