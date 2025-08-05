{
  pkgs,
  stevenblack-blocklist,
}:
# performing the relevant bits of: https://github.com/ScriptTiger/Hosts-BL
pkgs.runCommand "dns-blocklist" { } ''
  mkdir $out
  cat ${stevenblack-blocklist}/hosts |
    awk '/^0.0.0.0 [a-z]/ { print $2 }' |
    pr -9 -t -T -a -s' ' - |
    awk '{ print "0.0.0.0 " $0; print "::0 " $0 }' > $out/dns-blocklist
''
