alias emacs="emacsclient -c -n --alternate-editor=''"
alias finder="open -a finder ."

# Open dash docs from terminal
dash () {
  open dash://"$@"
}

# I'm running ztools a lot lately
infodump () {
  wine ~/vendor/ztools/infodump.exe "$@"
}
