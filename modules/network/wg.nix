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
      default = "wg.home:51930";
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
        group = "wheel";
      };

      networking.wg-quick.interfaces.wg0 = {
        inherit (cfg) autostart privateKeyFile;
        address = [
          "${cfg.ipv4}/32"
          "${cfg.ipv6}/128"
        ];
      };
    })

    (mkIf (cfg.enable && !isServer) {
      sops.secrets.wireguard-preshared-key = {
        mode = "0440";
        owner = config.users.users.root.name;
        group = "wheel";
      };

      networking.firewall.allowedUDPPorts = [ cfg.port ];
      networking.wg-quick.interfaces.wg0 = {
        peers = [
          {
            endpoint = if cfg.offLan then "wg.krh.me:51930" else cfg.endpoint;
            publicKey = cfg.serverPublicKey;
            presharedKeyFile = config.sops.secrets.wireguard-preshared-key.path;
            allowedIPs =
              [
                cfg.cidr
                cfg.cidr6
              ]
              ++ (lib.optionals cfg.offLan [
                net.lan.cidr
                net.lan.cidr6
              ]);
            persistentKeepalive = 25;
          }
        ];
      };
    })

    (mkIf (cfg.enable && !isServer && cfg.offLan) {
      networking.wg-quick.interfaces.wg0.dns = [
        net.wg.ipv6.pi
        net.wg.ipv4.pi
        net.wg.ipv6.htpc
        net.wg.ipv4.htpc
        "wg"
        "home"
      ];
    })

    (mkIf (cfg.enable && isServer) (
      let
        clients = removeAttrs net.wg.hosts [ config.networking.hostName ];

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

        peerSection = peer: ''
          [Peer]
          # friendly_name=${peer.name}
          PublicKey = ${peer.publicKey}
          AllowedIPs = ${head peer.allowedIPs}
        '';

        # Write a mock config the exporter can grab friendly names from
        conf = lib.strings.concatStrings (map peerSection peers);
        wgPeerNames = pkgs.writeText "wg-peer-names" conf;
      in
      {
        sops.secrets = psk-secrets;

        boot.kernel.sysctl = {
          # IPv4 was on without me explicitly opting in.
          # I have no idea why.
          "net.ipv6.conf.all.forwarding" = 1;
        };

        networking.nat = {
          enable = true;
          externalInterface = cfg.natInterface;
          internalInterfaces = [ "wg0" ];
        };

        networking.firewall = {
          allowedUDPPorts = [ cfg.port ];
          allowedTCPPorts = [
            config.services.prometheus.exporters.wireguard.port
          ];
        };

        networking.wg-quick.interfaces.wg0 = {
          listenPort = cfg.port;

          peers = lib.map (p: removeAttrs p [ "name" ]) peers;

          postUp = ''
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s ${cfg.cidr6} -o ${cfg.natInterface} -j MASQUERADE
          '';

          postDown = ''
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${cfg.cidr} -o ${cfg.natInterface} -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s ${cfg.cidr6} -o ${cfg.natInterface} -j MASQUERADE
          '';
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
