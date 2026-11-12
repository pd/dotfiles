{
  pkgs,
  stevenblack-blocklist,
  hagezi-lgwebos,
}:
# performing the relevant bits of: https://github.com/ScriptTiger/Hosts-BL
pkgs.runCommand "dns-blocklist" { } ''
  mkdir $out

  # hosts file syntax
  cat ${stevenblack-blocklist} |
    awk '/^0.0.0.0 [a-z]/ { print $2 }' |
    pr -9 -t -T -a -s' ' - |
    awk '{ print "0.0.0.0 " $0; print "::0 " $0 }' > $out/dns-blocklist

  # dnsmasq conf syntax
  cp ${hagezi-lgwebos} $out/lgwebos.conf
''
