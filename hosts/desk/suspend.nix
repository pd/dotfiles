{ pkgs, ... }:
{
  # USB wake from suspend (keyboard + root hubs only, not mouse)
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="1d6b", ATTR{power/wakeup}="enabled"
    ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="1818", ATTR{power/wakeup}="enabled"
  '';

  # save connected BT devices before suspend, reconnect after resume
  systemd.services.bt-save = {
    description = "Save connected Bluetooth devices before suspend";
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.bluez}/bin/bluetoothctl devices Connected | cut -d\" \" -f2 > /run/bt-connected'";
    };
  };

  systemd.services.bt-resume = {
    description = "Reconnect Bluetooth devices after resume";
    after = [ "suspend.target" ];
    wantedBy = [ "suspend.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.coreutils}/bin/sleep 3";
      ExecStart = "${pkgs.bash}/bin/bash -c 'while read mac; do ${pkgs.bluez}/bin/bluetoothctl connect \"$mac\"; done < /run/bt-connected'";
    };
  };
}
