# Things I'm guaranteed to want on every system.

{ pkgs, ... }: {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users = ["root" "@wheel"];
  security.sudo.wheelNeedsPassword = false;

  networking.firewall.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  programs = { zsh.enable = true; };
  environment.systemPackages = with pkgs; [vim git dig curl htop fd ripgrep];

  users = {
    mutableUsers = false;
    users.pd = {
      isNormalUser = true;
      extraGroups = ["networkmanager" "wheel"];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCgooJHfce7vOjmFCyWF/qqepTsQ1ohmESASroppNy4RiKHRVBWNqnMJDlSXfGlKOxVrnIMxcHAP9jbNhYT/x7/l8CBZvIngmNM2gcrFTwq5EF8QvTKc6Sp1dLJvOVXsmhJ2QmwJhzv2Q5LThKT8HMX3k9QAihppGjxAiFRZOGrmNhlIoEmRSRo3bjK5m3Z57WL640jLiYqzwG3YtLErToHCuMMgslkZsdPGoOnzX8bBpr/O1bYSqEDCm43p96AIf9KSeemZhztA/Kn/Cyjsj0d8egjJlLTbpFPR+3yoAT6YYO/bUmyYomZ/H29Dy2O//pgDCV6LlnARK8HlcZxRyid"
      ];
    };
  };

}
