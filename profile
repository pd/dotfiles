PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
PATH="/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$HOME/bin:$PATH"
export PATH

export PS1='\w $ '
export EDITOR=vi
export DISPLAY=:0.0

## generic
alias ls='ls -Fh'
alias ll='ls -l'
alias la='ls -a'
alias l='ls'
alias h='history'
alias bd='cd "$OLDPWD"'
alias ..='cd ..'
alias ...='cd ../..'
alias .p="source $HOME/.profile"

## depressingly generic
alias mt='mate .'

## for textmate bundles from svn with their shitty names.
export LC_CTYPE=en_US.UTF-8

## railsby
alias rails="ruby ~/vendor/rails/railties/bin/rails"
alias ss='./script/server'
alias sc='./script/console'
function cdgem {
  cd /usr/local/lib/ruby/gems/1.8/gems
  cd `ls | grep $1 | sort | tail -1`
}

## (c) Chapter Communications
alias ccmaster='mysql -h lwdb.madbytes.net -u ccmaster -p ccmaster'

## g blah -- grep -R 'blah' * | grep -v \.svn
## gf 'foo' '*.php' -- find all 'foo' in **/*.php
## svnrv -- svn revert all outstanding changes
function g  { grep -R "$1" * | grep -v \.svn; }
function gf { find . -iname "$2" -print0 | xargs -0 grep -Hn "$1"; }
function svnrv {
  if [ "x" != "x$*" ]; then
    svn revert $*
  else
    svn status | awk '{print $2}' | xargs svn revert
  fi
}

## emacs^[dd
set -o vi vi-tabcomplete

if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi
