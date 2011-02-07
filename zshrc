# -*- mode: sh -*-

path=(
  ~/bin
  ~/.cabal/bin
  ~/.seeds/bin
  /opt/java/bin
  /opt/local/bin /opt/local/sbin
  /usr/local/bin /usr/local/sbin
  /bin /sbin /usr/bin /usr/sbin
  .
)
fpath=(~/.zsh/functions $fpath)

## emacs^[dd
# bindkey -v
## vim<C-S-k>
bindkey -e

# but in terminal.app, vi mode is still the easiest:
[[ $TERM_PROGRAM = "Apple_Terminal" ]] && bindkey -v

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
for mod in prompt cli ruby git osx arch famous ; do
  source ~/.zsh/$mod.zsh
done
