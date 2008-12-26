## quarantine is lame
clear-xattr () {
  for a in `xattr $1`; do
    xattr -d $a $1
  done
}

## finder window in current directory
## maybe some day
alias finder='open -a finder .'
alias emacs='detach /Applications/Emacs.app/Contents/MacOS/Emacs -g 120x50'

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
    ln -s /opt/local/var/run/mysql5/mysqld.sock /tmp/mysql.sock
  fi
}

mount-apps () {
  [[ ! -d "$HOME/fs/apps" ]] && mkdir -p $HOME/fs/apps
  sshfs kyleh@lwapp.madbytes.net:/cc/apps $HOME/fs/apps -oreconnect,volname=apps
}
