path=(
  "$HOME/bin"
  '/opt/local/bin' '/opt/local/sbin'
  '/opt/local/lib/postgresql82/bin'
  '/opt/local/lib/erlang/bin'
  '/usr/local/mysql/bin'
  '/usr/local/bin' '/usr/local/sbin'
  '/bin' '/sbin' '/usr/bin' '/usr/sbin'
)
fpath+=("$HOME/.zsh/functions")

export PS1='kyleh %~ %# '
export EDITOR=vi
export DISPLAY=:0.0

## for textmate bundles from svn with their shitty names.
export LC_CTYPE=en_US.UTF-8

alias ls='ls -Fh'
alias ll='ls -l'
alias la='ls -a'
alias l='ls'
alias h='history'
alias p='ps axww'
alias scp='scp -C'
alias ..='cd ..'
alias ...='cd ../..'
alias .z='source ~/.zshrc'
alias cls='clear'

function mt {
  [ "$PWD" = "$HOME" ] && echo "not in $HOME." && return
  mate .
}

## ruby/rails misc.
alias sse='./script/server'
alias sst='./script/story'
alias sc='./script/console'
alias sg='./script/generate'
alias att='autotest'
alias rbst='ruby stories/all.rb'
alias gs='gem server'

## git. some to be used, some for reference.
alias gst='git status'
alias gbst='git branch -a -v'
alias gco='git checkout'
alias gb='git branch'
alias gsh='git show'
alias gshb='git show-branch'
alias gl='git log'
alias gm='git merge'
alias gd='git diff'
alias gds='git diff --cached'
alias gdhd='git diff HEAD'
alias gdst='git diff --stat'
alias stash='git stash'
alias unstash='git stash apply'
alias stash-ls='git stash list'
alias stash-patch='git stash show -p'

# doesn't work for merges, but generally it's good enough
function gci {
  if echo $PWD | grep 'chapcom' >/dev/null 2>&1; then
    command git-commit --author "Kyle Hargraves <kyleh@chaptercommunications.com>" $*
  else
    command git-commit $*
  fi
}

alias qg='open ~/bin/qgit.app'

## no spelling correction on certain commands
alias rm='nocorrect rm'
alias mv='nocorrect mv'
alias cp='nocorrect cp'

## quarantine is lame
function clear-xattr {
  for a in `xattr $1`; do
    xattr -d $a $1
  done
}

## wtfeh
alias finder='open -a finder .'

## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias digns1='dig @ns1.madbytes.net'

## util
function g { grep -R "$1" * | grep -v \.svn; }

## i don't know why the launchd file for these don't work
## but ffs it's horrid XML files and i don't care to learn.
function psqld {
  if [ "$1" = "stop" ]; then
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql82/defaultdb stop -s -m fast
  else
    sudo -u postgres pg_ctl -D /opt/local/var/db/postgresql82/defaultdb start -l /opt/local/var/log/postgresql82/postgres.log
  fi
}
function mysqld {
  if [ "$1" = "stop" ]; then
    /opt/local/share/mysql5/mysql/mysql.server stop
  else
    /opt/local/share/mysql5/mysql/mysql.server start
  fi
}

## expanded tabcomplete
autoload -U compinit
compinit

## iterm
##   \e]1 = tab label
##   \e]2 = window title
function iterm_set_tab_label {
  echo -ne "\e]1;$*\a"
}
function iterm_set_window_title {
  echo -ne "\e]2;$*\a"
}

# executed before printing a prompt
function precmd {
  host="kyleh"
  _pwd=`echo $PWD | sed "s,$HOME,~,"`
  str="$host: $_pwd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}
# executed just after reading a command, before running it
function preexec {
  host="kyleh"
  cmd=$(history $HISTCMD | cut -b7-)
  str="$host: $cmd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

## way too much to type
gems="/opt/local/lib/ruby/gems/1.8/gems"

## emacs^[dd
set -o vi vi-tabcomplete

## stores the pwd for the place the next shell will open.
## lame hack to let me hit ctrl+t for a new iterm tab and
## be in the same dir.
_qdir_tmpfile=~/.pd.qdir.tmp
function qdir {
  pwd > $_qdir_tmpfile
}

if [ -f $_qdir_tmpfile ]; then
  _qd=`cat $_qdir_tmpfile`
  [ -d $_qd ] && cd $_qd && rm $_qdir_tmpfile
fi
