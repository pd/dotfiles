{
  lib,
  config,
  net,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.wan;
  isServer = config.wan.natInterface != null;
in
{
  options = {
    wan.enable = mkEnableOption "on the wireguard wan";

    # wan.server.endpoint
    # wan.server.publicKey
    wan.endpoint = mkOption {
      type = types.str;
      default = "wg.krh.me:51820";
    };

    wan.serverPublicKey = mkOption {
      type = types.str;
      default = net.hosts.donix.wg.publicKey;
    };

    wan.cidr = mkOption {
      type = types.str;
      default = "10.100.100.0/24";
    };

    wan.ipv4 = mkOption { type = types.str; };

    wan.port = mkOption {
      type = types.int;
      default = 51820;
    };

    wan.publicKey = mkOption { type = types.str; };

    wan.natInterface = mkOption {
      type = types.nullOr types.str;
      default = null;
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
          listenPort = cfg.port;
          ips = [ "${cfg.ipv4}/32" ];
          privateKeyFile = config.sops.secrets.wireguard-private-key.path;
        };
      };

      networking.networkmanager.unmanaged = [ "wg0" ];
    })

    (mkIf (!isServer) {
      sops.secrets.wireguard-preshared-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "wheel";
      };

      networking.wireguard.interfaces.wg0.peers = [
        {
          endpoint = cfg.endpoint;
          allowedIPs = [ cfg.cidr ];
          publicKey = cfg.serverPublicKey;
          presharedKeyFile = config.sops.secrets.wireguard-preshared-key.path;
          persistentKeepalive = 25;
        }
      ];
    })

    (mkIf isServer (
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
          allowedIPs = [ "${peer.wg.ip}/32" ];
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
          peers = peers;
          postSetup = ''
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
          '';

          postShutdown = ''
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
          '';
        };

        services.prometheus.exporters.wireguard = {
          enable = true;
          listenAddress = net.hosts.donix.wg.ip;
          wireguardConfig = wgPeerNames;
        };

        networking.firewall.interfaces.wg0.allowedTCPPorts = [
          config.services.prometheus.exporters.wireguard.port
        ];
      }
    ))
  ];
}
