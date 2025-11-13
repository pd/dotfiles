{
  system = {
    primaryUser = "pd";

    defaults = {
      CustomUserPreferences = {
        "com.apple.Siri" = {
          ConfirmSiriInvokedViaEitherCmdTwice = 0;
          KeyboardShortcutSAE.enabled = 0;
          KeyboardShortcutPreSAE.enabled = 0;
          VoiceTriggerUserEnabled = 0;
        };

        "com.apple.assistant.support" = {
          "Assistant Enabled" = false; # go away siri
          "Dictation Enabled" = false;
        };

        "com.apple.desktopservices" = {
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };

        # Disable handoff
        "com.apple.coreservices.useractivityd" = {
          ActivityAdvertisingAllowed = false;
          ActivityReceivingAllowed = false;
        };
      };

      dock = {
        autohide = true;
        orientation = "bottom";
        mru-spaces = false;
        show-recents = false;
      };

      finder = {
        AppleShowAllExtensions = true;
        FXPreferredViewStyle = "Nlsv";
      };

      loginwindow = {
        autoLoginUser = "pd";
        GuestEnabled = false;
      };

      menuExtraClock = {
        Show24Hour = true;
        ShowDate = 1;
        ShowDayOfWeek = true;
        ShowSeconds = false;
      };

      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleInterfaceStyleSwitchesAutomatically = false;

        # stop pretending to be a phone
        "com.apple.swipescrolldirection" = false;

        # no beep when changing volume
        "com.apple.sound.beep.feedback" = 0;

        # 24h time
        AppleICUForce24HourTime = true;
      };

      trackpad = {
        TrackpadThreeFingerTapGesture = 0;
      };

      universalaccess = {
        reduceMotion = true;
        reduceTransparency = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    # shush
    startup.chime = false;
  };
}
