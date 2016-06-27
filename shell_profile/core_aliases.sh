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

up() {
  local op=print
  [[ -t 1 ]] && op=cd
  case "$1" in
    '') up 1;;
    -*|+*) $op ~$1;;
    <->) $op $(printf '../%.0s' {1..$1});;
    *) local -a seg; seg=(${(s:/:)PWD%/*})
       local n=${(j:/:)seg[1,(I)$1*]}
       if [[ -n $n ]]; then
         $op /$n
       else
         print -u2 up: could not find prefix $1 in $PWD
         return 1
       fi
  esac
}

if ! which realpath >/dev/null 2>&1; then
  if which greadlink >/dev/null 2>&1; then
    alias realpath='greadlink -m'
  else
    alias realpath='readlink -m'
  fi
fi
