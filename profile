# -*- mode: sh -*-

export PS1='\w $ '

# emacs mode everywhere
set -o emacs

# generic shell profile setup
source ~/dotfiles/shell_profile/*.sh
if [ -d ~/dotfiles/shell_profile/`uname -s` ]; then
  source ~/dotfiles/shell_profile/`uname -s`/*.sh
fi

# load global bash completion
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi
