# -*- mode: sh -*-
# zshrc: run for interactive shells

fpath=(~/.zsh/functions /usr/local/share/zsh-completions $fpath)

## emacs^[dd
# bindkey -v
## vim<C-S-k>
bindkey -e

# when i've ssh'd into something, stick with vi mode
[[ ! -z $SSH_CLIENT ]] && bindkey -v

## turn on expanded tabcomplete
autoload -U compinit
compinit

# `ls /foo/bar<C-w>` only kills 'bar'
autoload -U select-word-style
select-word-style bash

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

# one more, manually for now
source ~/dotfiles/shell_profile/bundler-exec.zsh

unfunction sourcedir

# simplistic git status in prompt
pd-git-prompt () {
  local ref=$(git symbolic-ref HEAD 2>/dev/null)
  if [ -z $ref ]; then return; fi
  if [[ -n $(git status -s 2>/dev/null) ]]; then
    local dirty=" δ"
  else
    local dirty=""
  fi
  echo " (${ref#refs/heads/}${dirty})"
}

# current ruby
pd-chruby-prompt () {
  if [ -n "$RUBY_ROOT" ]; then
    local rubyver=$(basename $RUBY_ROOT | sed 's/^ruby-//')
    echo " [chruby:$rubyver]"
  fi
}

# if this is over ssh, display the hostname to save my brain the effort
if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%~ @ %m » '
else
  export PS1='%~$(pd-chruby-prompt)$(pd-git-prompt) » '
fi

# In iTerm, set the tab title
precmd () {
  if [ -z "$INSIDE_EMACS" ]; then
    local tab_label="`print -P %m: %3~`"
    echo -ne "\e]2;${tab_label}\a" # set window title to full string
    echo -ne "\e]1;${tab_label}\a" # set tab title to rightmost 24 characters
  fi
}
