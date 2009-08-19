# vim:ft=sh

## emacs^[dd
set -o vi vi-tabcomplete

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
alias mt='mate .'

## for textmate bundles from svn with their shitty names.
export LC_CTYPE=en_US.UTF-8

if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi
