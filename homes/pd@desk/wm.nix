{
  pkgs,
  config,
  lib,
  ...
}:
let
  search-menu = pkgs.writeShellScriptBin "search-menu" (builtins.readFile ./search-menu.sh);

  screenshots =
    let
      grim = lib.getExe' pkgs.grim "grim";
      slurp = lib.getExe' pkgs.slurp "slurp";
      wl-copy = lib.getExe' pkgs.wl-clipboard "wl-copy";
    in
    pkgs.symlinkJoin {
      name = "wl-screenshots";
      paths = [
        (pkgs.writeShellScriptBin "wl-screenshot-region" ''
          ${grim} -g "$(${slurp})" - | ${wl-copy}
        '')

        (pkgs.writeShellScriptBin "wl-screenshot-display" ''
          ${grim} - | ${wl-copy}
        '')
      ];
    };
in
{
  home.packages = with pkgs; [
    grim
    lswt # to get app-id for riverctl rules
    imv # minimalist image viewer
    screenshots
    search-menu
    slurp
    swayidle
    sway-audio-idle-inhibit
    wl-clipboard
    wlr-randr
  ];

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

    icons = {
      enable = true;
      package = pkgs.numix-icon-theme;
      dark = "Numix";
      light = "Numix-Light";
    };

    cursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 24;
    };

    targets = {
      emacs.enable = false;
      firefox.enable = false;
      starship.enable = false;
    };
  };

  services.dunst.enable = true;

  programs.alacritty = {
    enable = true;
    settings = {
      mouse.hide_when_typing = true;

      # nas doesn't have alacritty in terminfo,
      # not worth fighting
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

      .modules-right {
        margin-right: 10px;
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
        "custom/dunst"
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

      tray = {
        spacing = 15;
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
          format =
            with config.lib.stylix.colors.withHashtag;
            let
              span = color: content: "<span color='${color}'>${content}</span>";
              bold = "<b>{}</b>";
              plain = "{}";
            in
            {
              months = span base0C bold;
              days = span base06 plain;
              weekdays = span base08 bold;
              today = span base0E bold;
            };
        };
        actions = {
          on-click-right = "mode";
          on-scroll-up = "tz_up";
          on-scroll-down = "tz_down";
        };
      };

      "custom/dunst" = {
        format = "{text}";
        exec = "${pkgs.pd.waybar-dunst}/bin/waybar-dunst";
        return-type = "json";
        on-click = "dunstctl history-pop";
        on-click-middle = "dunstctl history-clear";
        on-click-right = "dunstctl set-paused toggle";
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
        declare-mode = [
          "reshape"
        ];

        # Key names:
        # https://github.com/xkbcommon/libxkbcommon/blob/e9fd95/include/xkbcommon/xkbcommon-keysyms.h
        map.normal = {
          "${mod} Return" = "spawn alacritty";
          "${mod} Space" = "spawn 'fuzzel'";
          "${mod} slash" = "spawn '${search-menu}/bin/search-menu'";

          "${mod} R" = "enter-mode reshape";

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

          "None <print>" = "spawn 'wl-screenshot-region'";
          "${mod} <print>" = "spawn 'wl-screenshot-display'";

          "${mod}+Shift+Control BackSpace" = "exit";
        }
        // (lib.zipAttrs (map tag (lib.range 1 9)));

        map.reshape = {
          "None Enter" = "enter-mode normal";
          "None Return" = "enter-mode normal";

          "None Plus" = "send-layout-cmd rivertile 'main-count +1'";
          "None Minus" = "send-layout-cmd rivertile 'main-count +1'";

          "Shift Left" = "send-layout-cmd rivertile 'main-ratio -0.05'";
          "Shift Right" = "send-layout-cmd rivertile 'main-ratio +0.05'";

          "None Left" = "send-layout-cmd rivertile 'main-location left'";
          "None Up" = "send-layout-cmd rivertile 'main-location top'";
          "None Right" = "send-layout-cmd rivertile 'main-location right'";
          "None Down" = "send-layout-cmd rivertile 'main-location bottom'";
        };

        map-pointer.normal = {
          "${mod} BTN_LEFT" = "move-view";
          "${mod} BTN_RIGHT" = "resize-view";
          "${mod} BTN_MIDDLE" = "toggle-float";
        };

        input."pointer-*" = {
          accel-profile = "flat";
          pointer-accel = "0.1";
        };

        rule-add =
          let
            tag = app: v: "-app-id '${app}' tags '${toString v}'";
            float = app: "-app-id '${app}' float";

            tags = lib.mapAttrsToList tag {
              emacs = 3; # 1|2
              firefox = 5; # 1|3
              Slack = 8; # 4
            };

            floats = map float [
              "imv"
              "org.pulseaudio.pavucontrol"
              "com.github.PintaProject.Pinta"
            ];
          in
          tags ++ floats;

        spawn = map (cmd: "'${cmd}'") [
          "waybar"
          "rivertile -view-padding 2 -outer-padding 0"
          "sway-audio-idle-inhibit"
        ];

        background-color = lib.mkForce "0x6077a6";
        border-width = 3;
        default-layout = "rivertile";
        focus-follows-cursor = "normal";
      };
  };
}
