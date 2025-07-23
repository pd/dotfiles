# Original:
# https://github.com/akermu/emacs-libvterm/blob/988279316fc89e6d78947b48513f248597ba969a/etc/emacs-vterm-zsh.sh

# Modified to use precmd instead of chpwd hook because:
# - avoids running hook during shell startup
# - hook fires when an ssh session closes and the host prompt is rendered

function vterm_printf(){
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# With vterm_cmd you can execute Emacs commands directly from the shell.
# For example, vterm_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# vterm-eval-cmds.
vterm_cmd() {
  local vterm_elisp
  vterm_elisp=""
  while [ $# -gt 0 ]; do
    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
    shift
  done
  vterm_printf "51;E$vterm_elisp"
}

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt.
vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

# Keep emacs and terminals in sync with current directory
vterm_precmd_hook() {
  if [[ -n "$SSH_CONNECTION" ]]; then
    vterm_cmd update-default-directory "/ssh:$HOST:$PWD/"
  else
    vterm_cmd update-default-directory "$PWD/"
  fi
  print -Pn "\e]2;%m:%2~\a"
}

autoload -U add-zsh-hook
add-zsh-hook precmd vterm_precmd_hook
