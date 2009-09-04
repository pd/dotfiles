## quarantine is lame
clear-xattr () {
  for a in `xattr $1`; do
    xattr -d $a $1
  done
}

## finder window in current directory
## maybe some day
alias finder='open -a finder .'
