## quarantine is lame
clear-xattr () {
  for a in `xattr $1`; do
    xattr -d $a $1
  done
}

## finder window in current directory
## maybe some day
alias finder='open -a finder .'

## for some reason launchctl just doesn't actually start these things.
## they are configured, daemondo monitors them, but just doesn't run them.
psqld () {
  if [ "$1" = "stop" ]; then
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql83/defaultdb stop -s -m fast
  else
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql83/defaultdb start -l /opt/local/var/log/postgresql83/postgres.log
  fi
}
mysqld () {
  if [ "$1" = "stop" ]; then
    /opt/local/share/mysql5/mysql/mysql.server stop
  else
    /opt/local/share/mysql5/mysql/mysql.server start
  fi
}
