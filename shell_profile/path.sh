add_if () {
  [ -d $1 ] && PATH="$1:$PATH"
}

PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"

add_if /opt/local/bin
add_if /opt/local/sbin
add_if /usr/bin/perlbin/core
add_if /usr/bin/perlbin/vendor
add_if /usr/lib/perl5/vendor_perl/bin
add_if /usr/share/java/apache-ant/bin
add_if /opt/java/bin
add_if ~/.cabal/bin
add_if ~/local/bin

unfunction add_if
export PATH=~/bin:$PATH
