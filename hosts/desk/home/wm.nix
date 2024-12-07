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
      noto-fonts-cjk-sans
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

    fonts = {
      monospace = {
        name = "FiraCode Nerd Font";
        package = pkgs.fira-code;
      };
      sizes.terminal = 10;
    };
  };

  home-manager.users.pd =
    let
      search-menu = pkgs.writeShellScriptBin "search-menu" (builtins.readFile ./search-menu.sh);
    in
    {
      home.packages = with pkgs; [
        lswt # to get app-id for riverctl rules
        search-menu
        swayidle
        sway-audio-idle-inhibit
        wlr-randr
      ];

      stylix.targets = {
        firefox.enable = false;
        emacs.enable = false;
      };

      services.dunst.enable = true;

      programs.alacritty = {
        enable = true;
        settings = {
          mouse.hide_when_typing = true;

          # nas doesn't have alacritty in terminfo,
          # not worth figthing
          env.TERM = "xterm-256color";
        };
      };

      programs.fuzzel = {
        enable = true;
        settings = {
          main.font = lib.mkForce "Noto Sans:size=12";
          key-bindings = {
            execute-or-next = "Control+Tab";
            insert-selected = "Tab";
          };
        };
      };

      programs.waybar = {
        enable = true;

        style = ''
          * {
            font-family: Noto Sans Mono, NotoSans Nerd Font Mono, Symbols Nerd Font;
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
          modules-left = [
            "river/tags"
            "tray"
          ];

          modules-center = [ "river/window" ];

          modules-right = [
            "cpu"
            "memory"
            "network"
            "pulseaudio"
            "custom/audio-state"
            "clock"
          ];

          "river/tags" = {
            num-tags = 6;
            tag-labels = [
              "1:󰿗"
              "2:"
              "3:󰈹"
              "4:"
              "5"
              "6"
            ];
          };

          "river/window" = {
            max-length = 100;
          };

          cpu = {
            format = " {usage}%";
            tooltip = false;
          };

          memory = {
            format = " {percentage}%";
            tooltip-format = "";
          };

          network = {
            format-icons = [
              "󰤯"
              "󰤟"
              "󰤢"
              "󰤥"
              "󰤨"
            ];
            format-wifi = "{icon}";
            format-ethernet = "󰈀";
            format-disconnected = "⚠";
            tooltip-format-wifi = "WiFi: {essid} ({signalStrength}%)\n {bandwidthUpBytes}  {bandwidthDownBytes}";
            tooltip-format-ethernet = "Ethernet: {ifname}\n {bandwidthUpBytes}  {bandwidthDownBytes}";
            tooltip-format-disconnected = "Disconnected";
          };

          pulseaudio = {
            scroll-step = 2;
            on-click = "pavucontrol";
            format = "{icon} {volume}%";
            format-muted = "";
            format-icons = {
              default = [
                ""
                ""
              ];
            };
          };

          "custom/audio-state" = {
            format = "{icon}";
            exec = "sway-audio-idle-inhibit --dry-print-both-waybar";
            exec-if = "which sway-audio-idle-inhibit";
            return-type = "json";
            format-icons = {
              output = "";
              input = "";
              output-input = "  ";
              none = "";
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

      services.swayidle = {
        enable = true;
        extraArgs = [ "-d" ];
        systemdTarget = "river-session.target";
        timeouts = [
          {
            timeout = 600;
            command = "${pkgs.wlr-randr}/bin/wlr-randr --output DVI-I-1 --off";
            resumeCommand = "${pkgs.wlr-randr}/bin/wlr-randr --output DVI-I-1 --on";
          }
        ];
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
              "${mod} Space" = "spawn 'fuzzel'";
              "${mod} slash" = "spawn '${search-menu}/bin/search-menu'";

              "${mod}+Shift H" = "focus-view left";
              "${mod}+Shift J" = "focus-view down";
              "${mod}+Shift K" = "focus-view up";
              "${mod}+Shift L" = "focus-view right";
              "${mod}+Shift N" = "focus-view next";

              "${mod} comma" = "zoom";
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

            input."pointer-*" = {
              accel-profile = "flat";
              pointer-accel = "0.1";
            };

            rule-add = [
              "-app-id 'emacs' tags '3'" # 1|2
              "-app-id 'firefox' tags '5'" # 1|3
              "-app-id 'Slack' tags '8'" # 4
              "-app-id 'org.pulseaudio.pavucontrol' float"
            ];

            spawn = map (cmd: "'${cmd}'") [
              "waybar"
              "rivertile -view-padding 2 -outer-padding 0"
              "sway-audio-idle-inhibit"
            ];

            background-color = "0x6077a6";
            border-width = 3;
            default-layout = "rivertile";
            focus-follows-cursor = "normal";
          };
      };
    };
}
