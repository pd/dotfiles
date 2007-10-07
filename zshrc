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
  host="$HOST:t:t"
  pwd=`echo $PWD | sed "s,$HOME,~,"`
  str="$host: $pwd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}
# executed just after reading a command, before running it
function preexec {
  host="$HOST:t:t"
  cmd=$(history $HISTCMD | cut -b7-)
  str="$host: $cmd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

## emacs^[dd
set -o vi vi-tabcomplete