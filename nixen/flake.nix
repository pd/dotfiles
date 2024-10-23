{
  description = "nixen";

  # gh = repo: { url = "github:${repo}"; };
  # ghFollows = repo: (gh repo) // { inputs.nixpkgs.follows = "nixpkgs"; };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
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
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dotfiles = {
      url = "github:pd/dotfiles";
      flake = false;
    };
  };

  # TODO modular + home-manager as both integrated and distinct output
  # https://github.com/nmasur/dotfiles/blob/a6e4b3130d6f86303d9f80fdedbd38094d8427ac/flake.nix#L286-L307
  outputs =
    inputs@{
      self,
      nixpkgs,
      nixos-generators,
      home-manager,
      sops-nix,
      stylix,
      ...
    }: let
      # without this, `nixpkgs.lib` is inexplicably not found as soon as i switch
      # this to `rec`. wonky ass language. wat.
      lib = nixpkgs.lib;
    in {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;

      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
      ];

      # TODO push `nixosSystem` down and declare as `import ./hosts/foo { inherit inputs; ... }`
      nixosConfigurations = {
        # nixos-rebuild switch --flake .#donix --target-host donix --build-host donix --use-remote-sudo
        donix = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/donix
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-config.nix"
          ];
        };

        # nixos-rebuild switch --flake .#desk
        desk = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/desk
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
          ];
          specialArgs = {
            dotfiles = inputs.dotfiles;
          };
        };

        # building the SD, from desk with aarch64 emu:
        # nix run nixpkgs#nixos-generators -- -f sd-aarch64 --flake .#pi --system aarch64-linux -o ./pi.sd
        #
        # then deploying changes from desk:
        # nixos-rebuild test  --flake .#pi --target-host pi --use-remote-sudo --fast
        #
        # why --fast is critical, I don't know, but it is
        pi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            {
              nixpkgs.buildPlatform = "x86_64-linux";
              nixpkgs.hostPlatform = "aarch64-linux";
            }
            ./hosts/pi
            sops-nix.nixosModules.sops
          ];
        };
      };

      # homeConfigurations = {
      #   # For just tweaking home without touching the whole system
      #   # home-manager switch --flake .#desk
      #   desk = nixosConfigurations.desk.config.home-manager.users.pd.desk;
      # };

      images = {
        donix = nixos-generators.nixosGenerate {
          system = "x86_64-linux";
          format = "do";
          modules = [ ./hosts/donix ];
        };
      };
    };
}
