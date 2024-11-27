{
  system = {
    activationScripts.postUserActivation.text = ''
      # activateSettings -u will reload the settings from the database and
      # apply them to the current session, so we do not need to logout and
      # login again to make the changes take effect.
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

    defaults = {
      dock = {
        autohide = true;
        orientation = "bottom";
        mru-spaces = false;
        show-recents = false;
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
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    # shush
    startup.chime = false;
  };
}
