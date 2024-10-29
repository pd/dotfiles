{
  pkgs,
  config,
  ...
}:
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
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = config.users.users.pd.name;
      };

      initial_session = {
        command = "sway";
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
    home.packages = with pkgs; [
      alacritty
    ];

    programs.wofi.enable = true;

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
          "sway/workspaces"
          "sway/mode"
          "tray"
        ];

        modules-center = [
          "sway/window"
        ];

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

        "sway/workspaces" = {
          disable-scroll = true;
          persistent-workspaces = [ "1" "2" "3" "4" ];
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

    wayland.windowManager.sway = {
      enable = true;

      config = let
        mod = "Mod4";
        term = "${pkgs.alacritty}/bin/alacritty";
      in rec {
        modifier = mod;
        terminal = term;

        bars = [];

        keybindings = {
          "${mod}+Return" = "exec ${term}";
          "${mod}+Space" = "exec ${pkgs.wofi}/bin/wofi --show drun";

          "${mod}+r" = "mode resize";
          "${mod}+Shift+r" = "reload";
          "${mod}+Shift+q" = "kill";

          "${mod}+Shift+h" = "focus left";
          "${mod}+Shift+j" = "focus down";
          "${mod}+Shift+k" = "focus up";
          "${mod}+Shift+l" = "focus right";

          "${mod}+Shift+Ctrl+h" = "move left";
          "${mod}+Shift+Ctrl+j" = "move down";
          "${mod}+Shift+Ctrl+k" = "move up";
          "${mod}+Shift+Ctrl+l" = "move right";

          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";

          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
        };

        gaps = {
          smartBorders = "on";
          smartGaps = true;
        };

        window = {
          titlebar = false;
        };
      };
    };
  };
}
