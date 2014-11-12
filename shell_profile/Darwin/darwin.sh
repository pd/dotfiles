alias emacs="emacsclient -c -n --alternate-editor=''"
alias finder="open -a finder ."

# Open dash docs from terminal
dash () {
  open dash://"$@"
}

iterm () {
  local dir=$(pwd)

  if [ $# > 0 ]; then
    dir="$1"
  fi

  open -a iTerm "${dir}"
}
