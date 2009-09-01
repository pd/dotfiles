PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
PATH="/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$HOME/bin:$HOME/.cabal/bin:$PATH"
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
alias ....='cd ../../..'

alias emacsclient='emacsclient -n'
alias ec=emacsclient

alias gst='git status'
alias gba='git branch -a'
alias gbav='gba -v'

alias gl='git log'

alias gco='git checkout'
alias gci='git commit'
alias gb='git branch'
alias gm='git merge'
alias grb='git rebase'

alias gpl='git pull'
alias gps='git push'
alias gf='git fetch'
alias gru='git remote update'

alias gsh='git show'
alias gd='git diff'
alias gds='git diff --cached'
alias gdhd='git diff HEAD'
alias gdst='git diff --stat'

alias gsm='git submodule'

alias qg='detach qgit --all'
alias gx='gitx --all'

## for textmate bundles from svn with their shitty names.
export LC_CTYPE=en_US.UTF-8

if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi
