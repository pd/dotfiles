#!/bin/sh

ubus call dhcp ipv6leases |
  jsonfilter -e '$.device["br-lan"].leases[@.hostname="pi"]["ipv6-addr"][*].address' |
  grep -v fded:
