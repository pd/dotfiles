## quarantine is lame
clear-xattr () {
  for a in `xattr $1`; do
    xattr -d $a $1
  done
}

## finder window in current directory
## maybe some day
alias finder='open -a finder .'

## i don't know why the launchd file for these don't work
## but ffs it's horrid XML files and i don't care to learn.
psqld () {
  if [ "$1" = "stop" ]; then
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql82/defaultdb stop -s -m fast
  else
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql82/defaultdb start -l /opt/local/var/log/postgresql82/postgres.log
  fi
}
mysqld () {
  if [ "$1" = "stop" ]; then
    /opt/local/share/mysql5/mysql/mysql.server stop
  else
    /opt/local/share/mysql5/mysql/mysql.server start
  fi
}

## iterm
##   \e]1 = tab label
##   \e]2 = window title
iterm_set_tab_label () {
  echo -ne "\e]1;$*\a"
}
iterm_set_window_title () {
  echo -ne "\e]2;$*\a"
}
