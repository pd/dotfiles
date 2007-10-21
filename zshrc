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
alias mt='mate .'
alias scp='scp -C'
alias ..='cd ..'
alias ...='cd ../..'
alias .z='source ~/.zshrc'

## i dislike that git won't let me abbreviate
alias gst='git status'
alias gco='git checkout'
alias gci='git commit'
alias gb='git branch'
alias gsh='git show'
alias gshb='git show-branch'
alias gl='git log'
alias gd='git diff'
alias gdhd='git diff HEAD'
alias stash='git stash'
alias unstash='git stash apply'
alias stash-ls='git stash show'
alias stash-patch='git stash show -p'

## dir stack manipulation
alias d='dirs -v'

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
