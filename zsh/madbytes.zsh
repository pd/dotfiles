## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias dns1='dig @ns1.madbytes.net'

# allow 'cd ~cc/apps', 'cd ~oly'
cc=~/chapcom
oly=~/chapcom/apps/olympian
: ~cc ~oly

olyst () {
  if [ ! -d 'apps' -o ! -d '.git' ]; then
    echo "This doesn't look like the olympian repository."
    return 1
  fi

  echo olympian branches:
  git branch -a -v

  echo
  echo olympian status:
  git status

  echo
  echo olympian submodule status:
  git submodule status

  for app in 'apps/admin' 'apps/sites' 'apps/dashboard'; do
    echo
    echo $app branches:
    (cd $app; git branch -a -v)

    echo
    echo $app status:
    (cd $app; git status)
  done
}
