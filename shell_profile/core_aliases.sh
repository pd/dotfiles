alias ls='ls -Fh'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -al'
alias l='ls'
alias h='history'
alias p='ps axww'
alias scp='scp -C'
alias cls='clear'
alias sudo='command sudo '
alias g='git'
alias less='less -R'
alias pg='pgrep -fli'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

up () { for ((c=1; c <= $1; c++)); do cd ..; done }

if ! which realpath >/dev/null 2>&1; then
  if which greadlink >/dev/null 2>&1; then
    alias realpath='greadlink -m'
  else
    alias realpath='readlink -m'
  fi
fi
