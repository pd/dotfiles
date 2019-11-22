# -*- mode: sh -*-
# zshenv: run for all zsh shells

if [ -f ~/dotfiles/shell_profile/path.sh ]; then
  source ~/dotfiles/shell_profile/path.sh
fi

# chruby
for s in "/usr/local/share/chruby" "/usr/share/chruby"; do
  if [ -f "${s}/chruby.sh" ]; then
    source "${s}/chruby.sh"

    if [ -f ./.ruby-version ]; then
      chruby $(cat ./.ruby-version)
    elif [ -f ~/.ruby-version ]; then
      chruby $(cat ~/.ruby-version)
    fi
  fi
done

if [ $(command -v direnv 2>&1) ]; then
  eval "$(direnv hook zsh)"
fi

if [ -x /usr/local/bin/virtualenvwrapper.sh ]; then
  export WORKON_HOME=$HOME/.virtualenvs
  source /usr/local/bin/virtualenvwrapper.sh
fi

if [ -d "${HOME}/Library/Caskroom" ]; then
  export HOMEBREW_CASK_OPTS="--caskroom=${HOME}/Library/Caskroom"
fi
