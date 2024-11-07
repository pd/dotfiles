{ pkgs, ... }:
let
  etc = {
    "direnv/direnvrc" = ./etc/direnvrc;
    "irb/irbrc" = ./etc/irbrc;
    "pry/pryrc" = ./etc/pryrc;
    "pg/psqlrc" = ./etc/psqlrc;
    "sqlite3/sqliterc" = ./etc/sqliterc;
  };
in
{
  imports = [
    ./git.nix
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
        curl
        dig
        dyff
        fd
        google-cloud-sdk
        htop
        ipcalc
        just
        kfilt
        kubernetes-helm
        kustomize
        jq
        mise
        stern
        watchexec
      ]
      ++ (with unstable; [
        kubectl
        nil
        nixfmt-rfc-style
        sops
      ])
      ++ (lib.optionals stdenv.isDarwin [
        # these are currently installed globally via modules/core,
        # but I'm not using that (yet) on darwin:
        curl
        dig
        fd
        htop
        ripgrep
        tcpdump
        tree
        vim
      ]);
  };
}
