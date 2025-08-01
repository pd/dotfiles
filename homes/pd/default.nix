{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./files.nix
    ./git.nix
    ./k8s.nix
    ./music.nix
    ./nvim.nix
    ./prog.nix
    ./ssh.nix
    ./zsh.nix
  ];

  programs.home-manager.enable = true;

  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile =
    if pkgs.hostPlatform.isLinux then
      "${config.home.homeDirectory}/.config/sops/age/keys.txt"
    else
      "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt";

  news = {
    display = "silent";
    entries = lib.mkForce [ ];
  };

  home.username = "pd";
  home.packages =
    with pkgs;
    [
      age
      age-plugin-yubikey
      cfssl
      awscli2
      (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
      htop
      ipcalc
      just
      ssh-to-age
      ssm-session-manager-plugin
      unzip
      watchexec
    ]
    ++ (with unstable; [
      mise
      nil
      nixfmt-rfc-style
      opentofu
      postgresql
      sops
    ]);
}
