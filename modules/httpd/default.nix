{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.httpd;
in
{
  options.httpd = {
    enable = mkEnableOption "httpd";

    package = mkPackageOption pkgs "caddy" { };

    metrics = {
      port = mkOption {
        type = types.port;
        default = 2020;
      };

      cidrs = mkOption {
        description = "CIDRs to permit access to the metrics endpoint. Empty allows all.";
        type = types.listOf types.str;
        default = [ ];
      };
    };

    staticSites = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            root = mkOption { type = types.path; };
            extraConfig = mkOption {
              type = types.lines;
              default = "";
            };
          };
        }
      );
      default = { };
    };

    reverseProxies =
      let
        handler = types.submodule {
          options = {
            route = mkOption { type = types.str; };
            upstream = mkOption { type = types.str; };
          };
        };
        full = types.submodule {
          options = {
            handlers = mkOption {
              type = types.listOf handler;
              default = [ ];
            };
            cidrs = mkOption {
              description = "CIDRs to permit access. Empty allows all.";
              type = types.listOf types.str;
              default = [ ];
            };
            extraConfig = mkOption {
              type = types.lines;
              default = "";
            };
          };
        };
      in
      mkOption {
        type = types.attrsOf (types.either types.str full);
        default = { };
      };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      80
      443
      cfg.metrics.port
    ];

    users.users.pd.extraGroups = [ "caddy" ];

    services.caddy =
      let
        cidrFilter =
          cidrs:
          let
            joined = concatStringsSep " " cidrs;
          in
          if joined == "" then
            ""
          else
            ''
              @refused_cidr not client_ip ${joined}
              abort @refused_cidr
            '';

        metrics = {
          logFormat = "output discard";
          extraConfig = ''
            ${cidrFilter cfg.metrics.cidrs}
            metrics
          '';
        };

        accessLog = host: ''
          output file ${config.services.caddy.logDir}/access-${host}.log {
            mode 0640
            roll_size 100MiB
            roll_keep 2
          }
        '';

        reverseProxies = mapAttrs (
          host: upstream:
          (
            if lib.isString upstream then
              { extraConfig = "reverse_proxy ${upstream}"; }
            else
              let
                handle = h: "handle ${h.route} { reverse_proxy ${h.upstream} }";
                handlers = lib.map handle upstream.handlers;
              in
              {
                extraConfig = ''
                  ${cidrFilter upstream.cidrs}
                  ${lib.concatStringsSep "\n" handlers}
                  ${upstream.extraConfig}
                '';
              }
          )
          // ({ logFormat = accessLog host; })
        ) cfg.reverseProxies;

        staticSites = mapAttrs (host: site: {
          logFormat = accessLog host;
          extraConfig = ''
            root ${site.root}
            file_server
            ${site.extraConfig}
          '';
        }) cfg.staticSites;

        vhosts = {
          ":${toString cfg.metrics.port}" = metrics;
        }
        // staticSites
        // reverseProxies;
      in
      {
        enable = true;
        package = cfg.package;
        email = "letsencrypt@krh.me";

        globalConfig = ''
          metrics { per_host }
        '';

        virtualHosts = vhosts;
      };
  };
}
