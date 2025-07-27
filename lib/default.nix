{ dmerge, lib, ... }:
rec {
  pd = import ./pd { inherit lib; };

  uci = import ./uci {
    inherit dmerge lib;
    authorized-keys = pd.keys.workstations.ssh;
    resolvers = with pd.net.lan; [
      ipv6.pi
      ipv4.pi
      ipv6.htpc
      ipv4.htpc
    ];
  };
}
