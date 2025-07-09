{ net }:
{
  upnpd.config = {
    enabled = true;
    internal_iface = "lan";
    port = 5000;
    igdv1 = 1;
    uuid = "b3727d58-438d-4c7a-a63a-f33bc8cba7a6";
    upnp_lease_file = "/var/run/miniupnpd.leases";
    download = 1024;
    upload = 512;
  };

  perm_rule = [
    {
      action = "allow";
      ext_ports = "40000-65535";
      int_addr = net.lan.cidr;
      int_ports = "40000-65535";
      comment = "Allow high ports";
    }

    {
      action = "deny";
      ext_ports = "0-65535";
      int_addr = "0.0.0.0/0";
      int_ports = "0-65535";
      comment = "Default deny";
    }
  ];
}
