{
  dmerge,
  lib,
  ...
}:
rec {
  pd = import ./pd { inherit lib; };

  uci = import ./uci {
    inherit dmerge lib pd;
    authorized-keys = pd.keys.workstations.ssh;
  };
}
