{
  lib,
  config,
  pd,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.wg;
  isServer = config.wg.natInterface != null;
in
{
  options = {
    wg.enable = mkEnableOption "on the wireguard wan";

    wg.endpoint = mkOption {
      type = types.str;
      default = "wg.home:51930";
    };

    wg.serverPublicKey = mkOption {
      type = types.str;
      default = pd.net.hosts.pi.wg.publicKey;
    };

    wg.cidr = mkOption {
      type = types.str;
      default = pd.net.wg.cidr;
    };

    wg.cidr6 = mkOption {
      type = types.str;
      default = pd.net.wg.cidr6;
    };

    wg.ipv4 = mkOption { type = types.str; };
    wg.ipv6 = mkOption { type = types.str; };

    wg.port = mkOption {
      type = types.int;
      default = 51930;
    };

    wg.publicKey = mkOption { type = types.str; };

    wg.privateKeyFile = mkOption {
      type = types.str;
      default = config.sops.secrets.wireguard-private-key.path;
    };

    wg.presharedKeyFile = mkOption {
      type = types.str;
      default = config.sops.secrets.wireguard-preshared-key.path;
    };

    wg.natInterface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    wg.offLan = mkOption {
      type = types.bool;
      default = false;
    };

    wg.autostart = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      sops.secrets.wireguard-private-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "systemd-network";
      };

      environment.systemPackages = [ pkgs.wireguard-tools ];

      systemd.network = {
        enable = true;
        netdevs."30-wg0" = {
          netdevConfig = {
            Kind = "wireguard";
            Name = "wg0";
            MTUBytes = "1300";
          };

          wireguardConfig = {
            PrivateKeyFile = config.sops.secrets.wireguard-private-key.path;
          };
        };
      };
    })

    (mkIf (cfg.enable && !isServer) {
      sops.secrets.wireguard-preshared-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "systemd-network";
      };

      systemd.network = {
        netdevs."30-wg0".wireguardPeers = [
          {
            Endpoint = if cfg.offLan then "wg.krh.me:51930" else cfg.endpoint;
            PublicKey = cfg.serverPublicKey;
            PresharedKeyFile = config.sops.secrets.wireguard-preshared-key.path;
            AllowedIPs = [
              cfg.cidr
              cfg.cidr6
            ]
            ++ (lib.optionals cfg.offLan [
              pd.net.lan.cidr
              pd.net.lan.cidr6
            ]);
            PersistentKeepalive = 25;
          }
        ];

        networks.wg0 = {
          matchConfig.Name = "wg0";
          address = [
            "${cfg.ipv4}/24"
            "${cfg.ipv6}/64"
          ];
          dns = [
            pd.net.wg.ipv6.pi
            pd.net.wg.ipv6.htpc
            pd.net.wg.ipv4.pi
            pd.net.wg.ipv4.htpc
          ];
          domains = [
            "home"
            "wg"
          ];
          routes =
            if cfg.offLan then
              [
                { Destination = pd.net.lan.cidr; }
                { Destination = pd.net.lan.cidr6; }
              ]
            else
              [ ];
        };
      };
    })

    (mkIf (cfg.enable && isServer) (
      let
        clients = removeAttrs pd.net.wg.hosts [ config.networking.hostName ];

        psk-secrets = lib.mapAttrs' (name: peer: {
          name = "wireguard-preshared-key-${name}";
          value = {
            mode = "0440";
            owner = config.users.users.root.name;
            group = "systemd-network";
          };
        }) clients;

        peers = lib.mapAttrsToList (name: peer: {
          inherit name;
          AllowedIPs = [
            "${peer.wg.ipv4}/32"
            "${peer.wg.ipv6}/128"
          ];
          PublicKey = peer.wg.publicKey;
          PresharedKeyFile = config.sops.secrets."wireguard-preshared-key-${name}".path;
        }) clients;

        peerSection = peer: ''
          [Peer]
          # friendly_name=${peer.name}
          PublicKey = ${peer.PublicKey}
          AllowedIPs = ${head peer.AllowedIPs}
        '';

        # Write a mock config the exporter can grab friendly names from
        conf = lib.strings.concatStrings (map peerSection peers);
        wgPeerNames = pkgs.writeText "wg-peer-names" conf;
      in
      {
        sops.secrets = psk-secrets;

        boot.kernel.sysctl = {
          "net.ipv4.conf.all.forwarding" = 1;
          "net.ipv6.conf.all.forwarding" = 1;
        };

        networking.firewall = {
          allowedUDPPorts = [ cfg.port ];
          allowedTCPPorts = [
            config.services.prometheus.exporters.wireguard.port
          ];
        };

        systemd.network = {
          netdevs."30-wg0" = {
            wireguardConfig.ListenPort = cfg.port;
            wireguardPeers = lib.map (p: removeAttrs p [ "name" ]) peers;
          };

          networks."wg0" = {
            matchConfig.Name = "wg0";
            address = [
              "${cfg.ipv4}/24"
              "${cfg.ipv6}/64"
            ];
            networkConfig = {
              IPMasquerade = "both";
            };
          };
        };

        services.prometheus.exporters.wireguard = {
          enable = true;
          listenAddress = "0.0.0.0";
          wireguardConfig = wgPeerNames;
        };
      }
    ))
  ];
}
