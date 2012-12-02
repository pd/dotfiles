add_to_path_if_exists () {
  [ -d $1 ] && PATH="$1:$PATH"
}

PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"

add_to_path_if_exists /opt/local/bin
add_to_path_if_exists /opt/local/sbin
add_to_path_if_exists /usr/bin/perlbin/core
add_to_path_if_exists /usr/bin/perlbin/vendor
add_to_path_if_exists /usr/bin/core_perl
add_to_path_if_exists /usr/lib/perl5/vendor_perl/bin
add_to_path_if_exists /usr/share/java/apache-ant/bin
add_to_path_if_exists /usr/local/texlive/2011/bin/x86_64-darwin
add_to_path_if_exists /opt/java/bin
add_to_path_if_exists ~/.carton/bin
add_to_path_if_exists ~/.cabal/bin
add_to_path_if_exists ~/local/bin
add_to_path_if_exists ~/dotfiles/emacs.d/ecukes

export PATH=~/bin:$PATH
