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

## turn on expanded tabcomplete
autoload -U compinit
compinit

## emacs^[dd
set -o vi vi-tabcomplete

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

## no spelling correction on certain commands
alias rm='nocorrect rm'
alias mv='nocorrect mv'
alias cp='nocorrect cp'

## (c) MadBytes, LLC Exclusively Contracted with Chapter Communications, Inc.
alias digns1='dig @ns1.madbytes.net'

## util
function g { grep -R "$1" * | grep -v \.svn; }

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

## hey modularity
source ~/.zsh/ruby.zsh
source ~/.zsh/git.zsh
source ~/.zsh/osx.zsh

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
