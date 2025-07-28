{ pkgs, ... }:
{
  imports = [
    ./files.nix
    ./git.nix
    ./k8s.nix
    ./nvim.nix
    ./prog.nix
    ./zsh.nix
  ];

  programs.home-manager.enable = true;

  home.username = "pd";
  home.packages =
    with pkgs;
    [
      age
      age-plugin-yubikey
      cfssl
      awscli2
      ssm-session-manager-plugin
      (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
      htop
      ipcalc
      just
      ssh-to-age
      unzip
      watchexec
    ]
    ++ (with unstable; [
      mise
      nil
      nixfmt-rfc-style
      sops
    ]);
}
