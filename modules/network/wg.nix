{
  lib,
  config,
  net,
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
      default = "wg.krh.me:51930";
    };

    wg.serverPublicKey = mkOption {
      type = types.str;
      default = net.hosts.pi.wg.publicKey;
    };

    wg.cidr = mkOption {
      type = types.str;
      default = net.wg.cidr;
    };

    wg.cidr6 = mkOption {
      type = types.str;
      default = net.wg.cidr6;
    };

    wg.ipv4 = mkOption { type = types.str; };
    wg.ipv6 = mkOption { type = types.str; };

    wg.port = mkOption {
      type = types.int;
      default = 51930;
    };

    wg.publicKey = mkOption { type = types.str; };

    wg.natInterface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    wg.includeLanRoutes = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      sops.secrets.wireguard-private-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "wheel";
      };

      networking = {
        firewall.allowedUDPPorts = [ cfg.port ];
        wireguard.interfaces.wg0 = {
          ips = [
            "${cfg.ipv4}/32"
            "${cfg.ipv6}/128"
          ];
          privateKeyFile = config.sops.secrets.wireguard-private-key.path;
        };
      };

      networking.networkmanager.unmanaged = [ "wg0" ];
    })

    (mkIf (cfg.enable && !isServer) {
      sops.secrets.wireguard-preshared-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "wheel";
      };

      networking.wireguard.interfaces.wg0.peers = [
        {
          endpoint = cfg.endpoint;
          allowedIPs =
            [
              cfg.cidr
              cfg.cidr6
            ]
            ++ (lib.optionals cfg.includeLanRoutes [
              net.lan.cidr
              net.lan.cidr6
            ]);
          publicKey = cfg.serverPublicKey;
          presharedKeyFile = config.sops.secrets.wireguard-preshared-key.path;
          persistentKeepalive = 25;
        }
      ];
    })

    (mkIf (cfg.enable && isServer) (
      let
        hostname = config.networking.hostName;
        clients = removeAttrs net.wg.hosts [ hostname ];

        psk-secrets = lib.mapAttrs' (name: peer: {
          name = "wireguard-preshared-key-${name}";
          value = {
            mode = "0440";
            owner = config.users.users.root.name;
            group = "wheel";
          };
        }) clients;

        peers = lib.mapAttrsToList (name: peer: {
          inherit name;
          allowedIPs = [
            "${peer.wg.ipv4}/32"
            "${peer.wg.ipv6}/128"
          ];
          publicKey = peer.wg.publicKey;
          presharedKeyFile = config.sops.secrets."wireguard-preshared-key-${name}".path;
        }) clients;

        # Write a mock config the exporter can grab friendly names from
        conf = lib.strings.concatStrings (
          map (peer: ''
            [Peer]
            # friendly_name=${peer.name}
            PublicKey = ${peer.publicKey}
            AllowedIPs = ${head peer.allowedIPs}
          '') peers
        );
        wgPeerNames = pkgs.writeText "wg-peer-names" conf;
      in
      {
        sops.secrets = psk-secrets;

        networking.nat = {
          enable = true;
          externalInterface = cfg.natInterface;
          internalInterfaces = [ "wg0" ];
        };

        networking.wireguard.interfaces.wg0 = {
          listenPort = cfg.port;
          peers = peers;
          postSetup = ''
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s ${cfg.cidr6} -o ${cfg.natInterface} -j MASQUERADE
          '';

          postShutdown = ''
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s ${cfg.cidr6} -o ${cfg.natInterface} -j MASQUERADE
          '';
        };

        services.prometheus.exporters.wireguard = {
          enable = true;
          listenAddress = "0.0.0.0";
          wireguardConfig = wgPeerNames;
        };

        networking.firewall.allowedTCPPorts = [
          config.services.prometheus.exporters.wireguard.port
        ];
      }
    ))
  ];
}
