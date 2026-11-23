{
  pkgs,
  ...
}:
{
  system.stateVersion = 7;
  nixpkgs.hostPlatform.system = "aarch64-darwin";

  imports = [
    ./system.nix
    ./network.nix
    ./homebrew.nix
  ];

  networking.hostName = "armspan";
  users.users.pd.home = /Users/pd;

  networking.homeULA = {
    enable = true;
    interface = "en0";
  };

  security.pam.services.sudo_local.touchIdAuth = true;

  environment = {
    systemPackages = with pkgs; [
      wireguard-go
      wireguard-tools
      pd.wake-desk
    ];

    variables = {
      EDITOR = "nvim";
    };
  };
}
