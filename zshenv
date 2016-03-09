# zshenv: run for all zsh shells

# OSX's path_helper sucks. The /etc files for it seem to be overwritten
# god-knows-when. Just undo everything it decided to do and fkn start over.
export PATH="/usr/bin:/bin:/usr/sbin:/sbin"
if [ -f ~/dotfiles/shell_profile/path.sh ]; then
  source ~/dotfiles/shell_profile/path.sh
fi

# chruby
for s in "/usr/local/share/chruby" "/usr/share/chruby"; do
  if [ -f "${s}/chruby.sh" ]; then
    source "${s}/chruby.sh"

    [ -f ~/.ruby-version ] && chruby $(cat ~/.ruby-version)
    [ -f ./.ruby-version ] && chruby $(cat ./.ruby-version)
  fi
done

if [ $(command -v direnv 2>&1) ]; then
  eval "$(direnv hook zsh)"
fi

if [ -x /usr/local/bin/virtualenvwrapper.sh ]; then
  export WORKON_HOME=$HOME/.virtualenvs
  source /usr/local/bin/virtualenvwrapper.sh
fi
