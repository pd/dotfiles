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
alias less='less -r'
alias g='git'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

up () { for ((c=1; c <= $1; c++)); do cd ..; done }
