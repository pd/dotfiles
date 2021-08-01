alias emacs="emacsclient -c -n --alternate-editor=''"
alias finder="open -a finder ."

if [[ -x "${HOME}/Applications/MacVim.app/Contents/MacOS/Vim" ]]; then
  alias vim="${HOME}/Applications/MacVim.app/Contents/MacOS/Vim"
elif [[ -x "/Applications/MacVim.app/Contents/MacOS/Vim" ]]; then
  alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"
fi

# https://docs.brew.sh/Manpage#environment
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_INSTALL_CLEANUP=1
export HOMEBREW_NO_INSTALL_UPGRADE=1

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
