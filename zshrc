# -*- mode: sh -*-

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
export SAVEHIST=1000
export HISTFILE=~/.history.zsh
setopt histverify

## prompt
# expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

# emacs shell == "dumb"
# disable zsh line editing and let emacs do it
[[ $TERM = "dumb" ]] && unsetopt zle

# if this is over ssh, display the hostname to save my brain the effort
if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%~ @ %m » '
else
  export PS1='%~ » '
fi

## generic shell profile setup
function sourcedir () {
  if [ -d "$1" ]; then
    for f in "$1"/*.sh; do source $f; done
  fi
}

sourcedir ~/dotfiles/shell_profile
sourcedir ~/dotfiles/shell_profile/`uname -s`
sourcedir ~/dotfiles/private/shell_profile
sourcedir ~/dotfiles/private/shell_profile/`uname -s`

unfunction sourcedir
