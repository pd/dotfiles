# -*- mode: sh -*-

export PS1='\w $ '

# emacs mode everywhere
set -o emacs

# generic shell profile setup
for f in ~/dotfiles/shell_profile/*.sh; do
  source $f
done

if [ -d ~/dotfiles/shell_profile/`uname -s` ]; then
  for f in ~/dotfiles/shell_profile/`uname -s`/*.sh; do
    source $f
  done
fi

# load global bash completion
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
  . /usr/local/etc/bash_completion
fi
