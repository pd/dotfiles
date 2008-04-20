## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias dns1='dig @ns1.madbytes.net'

olyst () {
  if [ ! -d 'apps' -o ! -d '.git' ]; then
    echo This is not the olympian repository.
    return 1
  fi

  C="${fg[cyan]}"
  N="$terminfo[sgr0]"
  echo "$C"olympian"$N" status:
  git status

  echo
  echo "$C"olympian"$N" branches:
  git branch -a -v

  echo
  echo olympian submodule status:
  git submodule status

  for app in 'apps/admin' 'apps/sites' 'apps/dashboard'; do
    echo
    echo "$C"$app"$N" status:
    (cd $app; git status)

    echo
    echo "$C"$app"$N" branches:
    (cd $app; git branch -a -v)
  done
}

olygru () {
  if [ ! -d 'apps' -o ! -d '.git' ]; then
    echo This is not the olympian repository.
    return 1
  fi

  C="${fg[cyan]}"
  N="$terminfo[sgr0]"
  echo "$C"olympian-base"$N":
  git remote update

  for app in apps/*; do
    echo
    echo "$C"$app"$N":
    (cd $app; git remote update)
  done

  for plugin in plugins/*; do
    echo
    echo "$C"$plugin"$N":
    (cd $plugin; git remote update)
  done
}
