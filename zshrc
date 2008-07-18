path=(
  ~/bin
  /opt/local/bin /opt/local/sbin
  /opt/local/lib/postgresql83/bin
  /opt/local/lib/postgresql82/bin
  /opt/local/lib/erlang/bin
  /usr/local/mysql/bin
  /usr/local/bin /usr/local/sbin
  /bin /sbin /usr/bin /usr/sbin
  .
)
fpath=(~/.zsh/functions $fpath)

## emacs^[dd
bindkey -v

## turn on expanded tabcomplete
autoload -U compinit
compinit

## allows $fg[white], $bg[red], $terminfo[bold]
autoload colors zsh/terminfo
colors

setopt autocd     ## if i type '../somedir', just cd there.
setopt cdablevars ## 'cd foo' can be 'cd $foo' if 'foo' doesn't exist

## history
export HISTSIZE=500
export SAVEHIST=1000
export HISTFILE=~/.history.zsh
setopt histverify

## eh
export EDITOR=vim
export DISPLAY=:0.0
export LC_CTYPE=en_US.UTF-8

## hey modularity
for mod in prompt cli ruby git osx famous madbytes; do
  source ~/.zsh/$mod.zsh
done

## stores the pwd for the place the next shell will open.
## lame hack to let me hit ctrl+t for a new iterm tab and
## be in the same dir.
_qdir_tmpfile=~/.pd.qdir.tmp
qdir () {
  pwd > $_qdir_tmpfile
}
alias qd='qdir'

if [ -f $_qdir_tmpfile ]; then
  _qd=`cat $_qdir_tmpfile`
  [ -d $_qd ] && cd $_qd && rm $_qdir_tmpfile
fi
