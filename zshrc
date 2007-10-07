PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
PATH="/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$HOME/bin:$PATH"

PS1='kyleh %~ %# '
EDITOR=vi
DISPLAY=:0.0

alias ls='ls -Fh'
alias ll='ls -l'
alias la='ls -a'
alias l='ls'
alias .z='source ~/.zshrc'
alias mt='mate .'
alias scp='scp -C'

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

## expanded tabcomplete
autoload -U compinit
compinit

## iterm
function description { echo $HOST:r:r:$PWD }
function settab { echo -ne "\e]1;`description`\a" }
function settitle { echo -ne "\e]2;`description`\a" }
function chpwd { settab; settitle }
function precmd { settab; settitle }
function preexec { printf "\e]2;$HOST:$(history $HISTCMD | cut -b7- )\a" }

## emacs^[dd
set -o vi vi-tabcomplete
