{ lib, pkgs, ... }:
{
  lan.networkd = true;
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
  services.udev.extraRules =
    let
      ip = lib.getExe' pkgs.iproute2 "ip";
    in
    ''
      ACTION=="add", SUBSYSTEM=="net", ATTR{address}=="2c:f0:5d:db:8d:13", NAME=="*", \
        RUN+="${ip} link set dev $name address 2c:f0:5d:db:8d:f3", \
        RUN+="${ip} link set dev $name name eth0"

      ACTION=="add", SUBSYSTEM=="net", ATTR{address}=="14:cc:20:23:ea:6c", NAME=="*", \
        RUN+="${ip} link set dev $name address 14:cc:20:23:ea:fc", \
        RUN+="${ip} link set dev $name name wlan0"
    '';

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
