if [[ "$INSIDE_EMACS" == "vterm" ]]; then
  # https://github.com/akermu/emacs-libvterm/blob/f14d113ee4618f052879509ec378feb9766b871b/etc/emacs-vterm-zsh.sh
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
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

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

  autoload -U add-zsh-hook
  add-zsh-hook -Uz chpwd (){
    vterm_cmd update-default-directory "$PWD/"
    print -Pn "\e]2;%~\a"
  }

  vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
  }

  export PS1=$PS1"%{$(vterm_prompt_end)}"

  find-file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
  }
fi
