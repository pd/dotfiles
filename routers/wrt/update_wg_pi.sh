#!/bin/sh
grep 'pi$' /tmp/hosts/odhcpd | grep -v fded | cut -f1
