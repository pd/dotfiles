{ lib, pd, ... }:
let
  homeHosts = lib.map (h: "${h}.home") (
    [
      "wrt"
      "rpt"
    ]
    ++ pd.net.hosts.htpc.cnames
  );
in
{
  # get ff to stop showing "this connection is not secure ..."
  # warnings DIRECTLY ATOP MY PASSWORD MANAGER'S FILL PASSWORD UI
  programs.firefox.policies.Preferences."dom.securecontext.allowlist".Value =
    lib.concatStringsSep "," homeHosts;
}
