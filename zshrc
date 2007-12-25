path=(
  "$HOME/bin"
  '/opt/local/bin' '/opt/local/sbin'
  '/opt/local/lib/postgresql82/bin'
  '/usr/local/mysql/bin'
  '/usr/local/bin' '/usr/local/sbin'
  '/bin' '/sbin' '/usr/bin' '/usr/sbin'
)
fpath=( '/opt/local/share/zsh/4.2.6/functions' "$HOME/.zsh/functions" )

## make zsh maintain the dir stack
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent

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
alias ss='./script/server'
alias sc='./script/console'
alias att='autotest'
alias rbst='ruby stories/all.rb'
alias gs='gem server'

## git. some to be used, some for reference.
alias gst='git status'
alias gbst='git branch -a -v'
alias gco='git checkout'
alias gci='git commit'
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

alias qg='open ~/bin/qgit.app'

## no spelling correction on certain commands
alias rm='nocorrect rm'
alias mv='nocorrect mv'
alias cp='nocorrect cp'

## wtfeh
alias finder='open -a finder .'

## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias ccmaster='mysql -h db.madbytes.net -u ccmaster -p ccmaster'

## util
function g { grep -R "$1" * | grep -v \.svn; }
function gf { find . -iname "$2" -print0 | xargs -0 grep -Hn "$1"; }
function svnrv {
  if [ "x" != "x$*" ]; then
    svn revert $*
  else
    svn status | awk '{print $2}' | xargs svn revert
  fi
}

## % svnsum 1112 1118
## A blah/some.new.file
## U blah/some.updated.file
## etc.
function svnsum {
  svn diff --summarize -r "$1":"$2"
}

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
