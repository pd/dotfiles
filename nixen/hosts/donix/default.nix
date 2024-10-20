{
  pkgs,
  config,
  ...
}: let
  internetsfamous = pkgs.runCommand "install-nginx-site" {} ''
    install -Dm644 ${./internetsfamous/index.html} $out/www/index.html
  '';

in {
  imports = [
    ../../modules/base.nix
    ./users/pd.nix
  ];

  sops.defaultSopsFile = ./secrets.yaml;
  sops.secrets.wireguard-private-key = {
    mode  = "0440";
    owner = config.users.users.root.name;
    group = "wheel"; # TODO: can i get to this via config
  };

  system.stateVersion = "24.05";

  networking.hostName = "donix";
  networking.firewall = {
    allowedTCPPorts = [ 80 443 ];
    allowedUDPPorts = [ 51820 ];
  };

  networking.nat = {
    enable = true;
    externalInterface = "ens3";
    internalInterfaces = ["wg0"];
  };

  networking.wireguard.interfaces = {
    wg0 = {
      listenPort = 51820;
      ips = ["10.100.100.1/24"];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.100.0/24 -o ens3 -j MASQUERADE
      '';

      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.100.0/24 -o ens3 -j MASQUERADE
      '';

      peers = [
        { # desk
          publicKey = "CA7HMcgdgxixmpikVA15Bxydi7pFriBbR3A3W7mE2BU=";
          allowedIPs = ["10.100.100.10/32"];
        }

        { # pi
          publicKey = "gd9CT0OuYZP/kaFTP26uiIkXa+D/uvkAL0Z98X68Jx4=";
          allowedIPs = ["10.100.100.11/32"];
        }

        # htpc, nuc, et al
      ];
    };
  };

  time.timeZone = "America/Chicago";

  users.users = {
    rhys = {
      isNormalUser = true;
      extraGroups = ["networkmanager" "wheel" "keys"];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh"
      ];
    };
  };

  services.nginx = {
    enable = true;
    statusPage = false;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedBrotliSettings = true;
    recommendedGzipSettings = true;
    recommendedZstdSettings = true;

    virtualHosts."donix.krh.me" = {
      enableACME = true;
      forceSSL = true;
      root = "${internetsfamous}/www";
    };
  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "letsencrypt@krh.me";

  environment.systemPackages = [internetsfamous];
}
