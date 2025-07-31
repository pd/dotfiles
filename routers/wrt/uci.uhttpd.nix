{ pd, ... }:
let
  inherit (pd.net) lan;
in
{
  uhttpd.main = {
    listen_http = [
      "${lan.ipv4.wrt}:80"
      "[${lan.ipv6.wrt}]:80"
    ];
    redirect_https = false;

    home = "/www";
    cgi_prefix = "/cgi-bin";
    lua_prefix = [
      "/cgi-bin/luci=/usr/lib/lua/luci/sgi/uhttpd.lua"
    ];
    ubus_prefix = "/ubus";

    rfc1918_filter = true;
    no_dirlists = true;
  };
}
