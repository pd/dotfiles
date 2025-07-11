{ pkgs, ... }:
let
  etc = {
    "direnv/direnvrc" = ./etc/direnvrc;
    "irb/irbrc" = ./etc/irbrc;
    "mise/config.toml" = ./etc/mise.toml;
    "mise/go" = ./etc/mise-go-packages;
    "pry/pryrc" = ./etc/pryrc;
    "pg/psqlrc" = ./etc/psqlrc;
    "sqlite3/sqliterc" = ./etc/sqliterc;
    "zsh/p10k.zsh" = ./etc/p10k.zsh;
  };
in
{
  imports = [
    ./bin.nix
    ./git.nix
    ./k8s.nix
    ./nvim.nix
    ./prog.nix
    ./ssh.nix
    ./zsh.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  home-manager.users.pd = {
    programs.home-manager.enable = true;

    xdg.configFile = builtins.mapAttrs (dest: source: { inherit source; }) etc;

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
  };
}
