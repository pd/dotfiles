{ pkgs, config, lib, ... }:
{
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    liberation_ttf
    nerdfonts
    proggyfonts
  ];

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
          font-family: FiraCode Nerd Font Mono, Noto Sans Mono;
          font-size: 16px;
        }
      '';

      settings.mainBar = {
        modules-left = [
          "river/tags"
          "river/mode"
          "river/layout"
        ];

        modules-center = [ "river/window" ];

        modules-right = [
          "cpu"
          "memory"
          "network#lan"
          "network#wg"
          "pulseaudio"
          "clock"
        ];

        cpu = {
          format = " {usage}%";
        };

        memory = {
          format = " {percentage}%";
        };

        "river/workspaces" = {
          disable-scroll = true;
          persistent-workspaces = {
            "1" = [ ];
            "2" = [ ];
            "3" = [ ];
            "4" = [ ];
          };
        };

        "network#lan" = {
          name = "lan";
          format = "<span color='#69ff94'></span> {essid}/{signalStrength}%";
          format-disconnected = "<span color='#dd532e'></span>";
          tooltip-format = "{ifname}: {ipaddr}/{cidr} via {gwaddr}";
        };

        "network#wg" = {
          name = "wg";
          format = "<span color='#69ff94'></span>";
          format-disconnected = "<span color='#dd532e'></span>";
          tooltip-format = "{ifname}: {ipaddr}/{cidr} via {gwaddr}";
        };

        clock = {
          timezones = [
            "America/Chicago"
            "UTC"
          ];
          format = " {:%Z|%F %H:%M}";
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

      settings = let
        mod = "Super";
        tag = index:
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
          "waybar"
          "rivertile -view-padding 2 -outer-padding 0"
        ];

        border-width = 3;
        default-layout = "rivertile";
        focus-follows-cursor = "always";
      };
    };
  };
}
