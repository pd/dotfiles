{
  pkgs,
  config,
  lib,
  ...
}:
{
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      fira-code
      fira-code-symbols
      liberation_ttf
      nerdfonts
      proggyfonts
    ];

    fontconfig = {
      defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Noto Sans" ];
        emoji = [ "Noto Color Emoji" ];
        monospace = [ "FiraCode Nerd Font" ];
      };
    };
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # caps -> ctrl
  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "layer(control)";
          };
        };
      };
    };
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd river";
        user = config.users.users.pd.name;
      };

      initial_session = {
        command = "river";
        user = config.users.users.pd.name;
      };
    };
  };

  stylix = {
    enable = true;
    polarity = "dark";
    image = config.lib.stylix.pixel "base03";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ashes.yaml";
  };

  home-manager.users.pd = {
    home.packages = with pkgs; [ alacritty ];

    programs.wofi.enable = true;
    services.dunst.enable = true;

    programs.waybar = {
      enable = true;

      style = ''
        * {
          font-family: Noto Sans Mono;
          font-size: 11pt;
        }

        .modules-right label {
          margin: 5px;
        }

        #tags .focused {
          background-color: @base01;
        }

        #tags .occupied {
          color: @base0C;
        }

        #tags .urgent {
          color: @base0F;
        }

        #network.disconnected {
          color: @base0F;
        }
      '';

      settings.mainBar = {
        modules-left = [ "river/tags" ];

        modules-center = [ "river/window" ];

        modules-right = [
          "cpu"
          "memory"
          "network#lan"
          "network#wg"
          "pulseaudio"
          "clock"
        ];

        "river/tags" = {
          num-tags = 4;
        };

        cpu = {
          format = " {usage}%";
          tooltip = false;
        };

        memory = {
          format = " {percentage}%";
          tooltip-format = "";
        };

        "network#lan" = {
          name = "lan";
          format = "";
          tooltip-format-wifi = "{ifname} {ipaddr}/{cidr}\n{essid} ({signalStrength}%)";
        };

        "network#wg" = {
          name = "wg";
          interface = "wg0";
          format = "🔒";
          format-disconnected = "";
          tooltip-format = "{ifname} {ipaddr}/{cidr}";
        };

        pulseaudio = {
          scroll-step = 2;
          format = "{icon} {volume}%";
          format-muted = "";
          format-icons = {
            default = [
              ""
              ""
            ];
          };
        };

        clock = {
          timezones = [
            "America/Chicago"
            "UTC"
          ];
          format = "{:%F %H:%M %Z}";
          tooltip-format = "<tt>{calendar}</tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "";
            on-scroll = 1;
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "tz_up";
            on-scroll-down = "tz_down";
          };
        };
      };
    };

    wayland.windowManager.river = {
      enable = true;
      systemd.enable = true;

      settings =
        let
          mod = "Super";
          tag =
            index:
            let
              i = toString index;
              tags = "$((1 << (${i} - 1)))";
            in
            {
              "${mod} ${i}" = "set-focused-tags ${tags}";
              "${mod}+Shift ${i}" = "set-view-tags ${tags}";
              "${mod}+Control ${i}" = "toggle-focus-tags ${tags}";
              "${mod}+Control+Shift ${i}" = "toggle-view-tags ${tags}";
            };
        in
        {
          map.normal = {
            "${mod} Return" = "spawn alacritty";

            "${mod} Space" = "spawn 'wofi --show drun'";
            "${mod} comma" = "zoom";

            "${mod}+Shift H" = "focus-view left";
            "${mod}+Shift J" = "focus-view down";
            "${mod}+Shift K" = "focus-view up";
            "${mod}+Shift L" = "focus-view right";
            "${mod}+Shift N" = "focus-view next";

            "${mod}+Shift+Control J" = "swap previous";
            "${mod}+Shift+Control K" = "swap next";

            "${mod}+Shift F" = "toggle-fullscreen";
            "${mod}+Shift Space" = "toggle-float";

            "${mod}+Alt I" = "send-layout-cmd rivertile 'main-count +1'";
            "${mod}+Alt D" = "send-layout-cmd rivertile 'main-count -1'";
            "${mod}+Alt bracketleft" = "send-layout-cmd rivertile 'main-ratio -0.05'";
            "${mod}+Alt bracketright" = "send-layout-cmd rivertile 'main-ratio +0.05'";
            "${mod}+Alt Left" = "send-layout-cmd rivertile 'main-location left'";
            "${mod}+Alt Up" = "send-layout-cmd rivertile 'main-location top'";
            "${mod}+Alt Right" = "send-layout-cmd rivertile 'main-location right'";
            "${mod}+Alt Down" = "send-layout-cmd rivertile 'main-location bottom'";

            "${mod}+Shift+Control BackSpace" = "exit";
          } // (lib.zipAttrs (map tag (lib.range 1 9)));

          map-pointer.normal = {
            "${mod} BTN_LEFT" = "move-view";
            "${mod} BTN_RIGHT" = "resize-view";
            "${mod} BTN_MIDDLE" = "toggle-float";
          };

          spawn = [
            "'waybar'"
            "'rivertile -view-padding 2 -outer-padding 0'"
          ];

          background-color = "0x6077a6";
          border-width = 3;
          default-layout = "rivertile";
          focus-follows-cursor = "normal";
        };
    };
  };
}
