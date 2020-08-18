alias ls='ls -Fh'
alias sudo='command sudo '
alias g='git'
alias less='less -R'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

genpass() {
  LC_ALL=C tr -dc '0-9A-Za-z_@#%*,.:?!~$^&()[]' < /dev/urandom | head -c${1:-20}
  echo
}

if ! which realpath >/dev/null 2>&1; then
  if which greadlink >/dev/null 2>&1; then
    alias realpath='greadlink -m'
  else
    alias realpath='readlink -m'
  fi
fi

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

man() {
  env \
    LESS_TERMCAP_md=$'\e[1;36m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[1;32m' \
      man "$@"
}
