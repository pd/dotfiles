alias emacs="emacsclient -c -n --alternate-editor=''"
alias finder="open -a finder ."

if [[ -x "${HOME}/Applications/MacVim.app/Contents/MacOS/Vim" ]]; then
  alias vim="${HOME}/Applications/MacVim.app/Contents/MacOS/Vim"
elif [[ -x "/Applications/MacVim.app/Contents/MacOS/Vim" ]]; then
  alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"
fi

export HOMEBREW_CASK_OPTS="--appdir=~/Applications"

# Open dash docs from terminal
dash () {
  open dash://"$@"
}

iterm () {
  local dir=$(pwd)

  if [ $# -gt 0 ]; then
    dir="$1"
  fi

  open -a iTerm "${dir}"
}
