{ ... }:
{
  lan.wired.interface = "eth0";

  # keep wifi on as a lower priority interface.
  # mostly just for giggles, tbh.
  lan.wifi.interface = "wlan0";
  lan.wifi.requiredForOnline = false;
  lan.wifi.id = {
    # set a duid distinct from the wired interface;
    # odhcpd doesn't respect iaid.
    duid = "de:ad:c0:de:ca:fe";
    iaid = 15503; # via $((RANDOM))
  };

  # Reassign MACs so nixos is distinct from windows
  systemd.network.links."10-eth0" = {
    matchConfig.PermanentMACAddress = "2c:f0:5d:db:8d:13";
    linkConfig = {
      MACAddressPolicy = "none";
      MACAddress = "2c:f0:5d:db:8d:f3";
      Name = "eth0";
    };
  };

  systemd.network.links."10-wlan0" = {
    matchConfig.PermanentMACAddress = "14:cc:20:23:ea:6c";
    linkConfig = {
      MACAddress = "14:cc:20:23:ea:fc";
      MACAddressPolicy = "none";
      Name = "wlan0";
    };
  };

  # And wait for reassignment to complete before bringing the network up,
  # so we get the right DHCP assignments.
  #
  # NixOS had but then removed this:
  #   https://github.com/NixOS/nixpkgs/pull/107382/files#diff-c8a612b3ca275d5797f396ca2c5348e6e77099a9acc085d64cc3a028f52a38a8
  #
  # Supposedly it shouldn't be necessary any more (see PR comments), but
  # without it the mac addr reassignment has like 50% odds of working at boot.
  systemd.services.systemd-networkd = {
    requires = [ "systemd-udev-settle.service" ];
    after = [ "systemd-udev-settle.service" ];
  };

  # For when you can't figure out wtf networkd is doing:
  # systemd.services.systemd-networkd.environment.SYSTEMD_LOG_LEVEL = "debug";
}
