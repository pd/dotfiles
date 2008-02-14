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

## emacs^[dd
set -o vi vi-tabcomplete

## turn on expanded tabcomplete
autoload -U compinit
compinit

export EDITOR=vim
export DISPLAY=:0.0
export LC_CTYPE=en_US.UTF-8

## hey modularity
for mod in prompt cli ruby git osx madbytes; do
  source ~/.zsh/$mod.zsh
done

## redefine gci just for me eh
# doesn't work for merges, but generally it's good enough
unalias gci
gci () {
  if echo $PWD | grep 'chapcom' >/dev/null 2>&1; then
    command git-commit --author "Kyle Hargraves <kyleh@chaptercommunications.com>" $*
  else
    command git-commit $*
  fi
}

## stores the pwd for the place the next shell will open.
## lame hack to let me hit ctrl+t for a new iterm tab and
## be in the same dir.
_qdir_tmpfile=~/.pd.qdir.tmp
qdir () {
  pwd > $_qdir_tmpfile
}

if [ -f $_qdir_tmpfile ]; then
  _qd=`cat $_qdir_tmpfile`
  [ -d $_qd ] && cd $_qd && rm $_qdir_tmpfile
fi
