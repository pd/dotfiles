alias emacs="open /Applications/Emacs.app"
alias finder="open -a finder ."

if [ -d /var/log/asl ]; then
  sudo rm -f /var/log/asl/*.asl
fi
