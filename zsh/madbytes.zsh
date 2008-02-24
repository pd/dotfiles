## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias dns1='dig @ns1.madbytes.net'

_single_o2st () {
  proj=$1
  echo -n "$proj @ "
  (cd $proj;
   git-show --abbrev-commit --pretty=format:'%h' | head -1)

  [[ "ambrosia" = $proj ]] && return

  echo -n "  w/ambrosia @ "
  (cd $proj/vendor/plugins/ambrosia;
   git-show --abbrev-commit --pretty=format:'%h' | head -1)
}
o2st () {
  for d in o2 ambrosia olympian; do
    [[ ! -d $d ]] && echo "Can't find $d repository" && return
  done

  _single_o2st ambrosia
  echo
  _single_o2st o2
  echo
  _single_o2st olympian
}
