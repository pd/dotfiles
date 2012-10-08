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

if [ -f /usr/share/vim/vim73/macros/less.sh ]; then
  alias less='sh /usr/share/vim/vim73/macros/less.sh'
else
  alias less='less -R'
fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

up () { for ((c=1; c <= $1; c++)); do cd ..; done }
