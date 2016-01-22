# zshenv: run for all zsh shells

# OSX's path_helper sucks. The /etc files for it seem to be overwritten
# god-knows-when. Just undo everything it decided to do and fkn start over.
export PATH="/usr/bin:/bin:/usr/sbin:/sbin"
if [ -f ~/dotfiles/shell_profile/path.sh ]; then
  source ~/dotfiles/shell_profile/path.sh
fi

# chruby
if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh

  [ -f ~/.ruby-version ] && chruby $(cat ~/.ruby-version)
  [ -f ./.ruby-version ] && chruby $(cat ./.ruby-version)
fi

if [ $(command -v direnv 2>&1) ]; then
  eval "$(direnv hook zsh)"
fi

if [ -x /usr/local/bin/virtualenvwrapper.sh ]; then
  export WORKON_HOME=$HOME/.virtualenvs
  source /usr/local/bin/virtualenvwrapper.sh
fi
