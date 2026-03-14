{
  dmerge,
  lib,
  pd,

  authorized-keys,
}:
with lib;
{
  bridgeLan =
    n: macaddr:
    let
      devs = map (i: {
        inherit macaddr;
        name = "lan${toString i}";
      }) (range 1 n);
    in
    devs
    ++ [
      {
        name = "br-lan";
        type = "bridge";
        ports = catAttrs "name" devs;
      }
    ];

  wifi = rec {
    bands = {
      "2g" = {
        band = "2g";
        channel = 1;
        htmode = "HE40";
      };
      "5g" = {
        band = "5g";
        channel = 36;
        htmode = "HE80";
      };
    };

    device = path: {
      inherit path;
      type = "mac80211";
      country = "US";
      cell_density = "1";
    };

    iface = device: {
      inherit device;
      network = "lan";
      ssid = pd.net.wifi.ssid;
      encryption = "psk2";
      key._secret = "wifi_password";
    };

    sta = device: (iface device) // { mode = "sta"; };
    ap =
      device:
      (iface device)
      // {
        mode = "ap";
        ieee80211r = true;
        ieee80211w = 1; # PMF optional
        ft_over_ds = false;
        mobility_domain = "dead";
      };

    off = {
      disabled = true;
    };

    wds = {
      wds = true;
    };
  };

  mkRouter =
    hostname: secrets: custom:
    dmerge.merge {
      deploy = {
        host = "${hostname}.home";
        sshConfig.Port = 1222;
      };

      sopsSecrets = secrets;
      users.root.hashedPasswordSecret = "root_password";
      etc."dropbear/authorized_keys".text = concatStringsSep "\n" authorized-keys;

      uci.settings = {
        dropbear.dropbear = map (iface: {
            Interface = iface;
            Port = 1222;
            PasswordAuth = "off";
          }) [ "lan" "lan6" ];

        system = {
          system = [
            {
              inherit hostname;
              zonename = "America/Chicago";
            }
          ];

          timeserver.ntp = {
            enabled = true;
            enable_server = false;
            server = [
              "0.openwrt.pool.ntp.org"
              "1.openwrt.pool.ntp.org"
              "2.openwrt.pool.ntp.org"
              "3.openwrt.pool.ntp.org"
            ];
          };
        };

        network = {
          interface.loopback = {
            device = "lo";
            proto = "static";
            ipaddr = "127.0.0.1";
            netmask = "255.0.0.0";
          };

          interface.lan = {
            device = "br-lan";
            proto = "static";
            netmask = pd.net.lan.netmask;
            dns = pd.net.resolvers pd.net.lan;
            dns_search = [ "home" ];
          };
        };

        firewall = {
          defaults = [
            {
              input = "REJECT";
              output = "ACCEPT";
              forward = "REJECT";
              synflood_protect = true;
            }
          ];

          zone = [
            {
              name = "lan";
              input = "ACCEPT";
              output = "ACCEPT";
              forward = "ACCEPT";
              network = [ "lan" ];
              log = false;
            }

            {
              name = "wan";
              input = "REJECT";
              output = "ACCEPT";
              forward = "REJECT";
              network = [
                "wan"
                "wan6"
              ];
              masq = true;
              mtu_fix = true;
              log = false;
            }
          ];

          forwarding.lan_wan = {
            src = "lan";
            dest = "wan";
          };

          rule = import ./default-firewall-rules.nix;
        };

        prometheus-node-exporter-lua = {
          prometheus-node-exporter-lua.main = {
            listen_interface = "lan";
            listen_ipv6 = true;
            listen_port = 9100;
          };
        };

        usteer.usteer = [
          {
            network = "lan";
            signal_diff_threshold = 8;
            roam_scan_snr = "-65";
          }
        ];
      };

      # TODO: Should not really be reaching into deploySteps at all,
      # it's explicitly marked internal. still doing it.

      # Install just enough for a decent zsh env that works via
      # tramp+vterm.
      deploySteps.zsh = {
        priority = 100;
        copy = "";
        apply = ''
          cat >/root/.profile <<EOF
          if [[ "\$TERM" == "dumb" ]]; then
            export PS1='$ '
          elif which zsh >/dev/null 2>&1; then
            exec zsh -il
          fi
          EOF

          cat >/root/.zshrc <<EOF
          export PROMPT='%n@%m:%~/ > '
          EOF
        '';
      };

      # I don't want dewclaw managing packages at all.
      deploySteps.packages = {
        copy = mkForce "";
        apply = mkForce "";
      };

    } custom;
}
