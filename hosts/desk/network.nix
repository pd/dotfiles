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
  # Ethernet is easy and we can do it with a networkd .link:
  systemd.network.links."10-eth0" = {
    matchConfig.PermanentMACAddress = "2c:f0:5d:db:8d:13";
    linkConfig = {
      MACAddressPolicy = "none";
      MACAddress = "2c:f0:5d:db:8d:f3";
      Name = "eth0";
    };
  };

  # wifi is more bothersome; iwd seizes it basically the second it
  # shows up, so when a .link fires milliseconds later, it gets
  # "device busy" and fails to set the MAC.
  #
  # Gotta manually set up /var/lib/iwd/bazqux.psk:
  #
  #   [Settings]
  #   AddressOverride=14:cc:20:23:ea:fc
  #
  # This AddressRandomization just tells iwd to honor that.
  # https://man.archlinux.org/man/extra/iwd/iwd.network.5.en
  networking.wireless.iwd.settings = {
    General.AddressRandomization = "network";
  };

  # For when you can't figure out wtf networkd is doing:
  # systemd.services.systemd-networkd.environment.SYSTEMD_LOG_LEVEL = "debug";
}
