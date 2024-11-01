# -*- mode: sh -*-
# zshrc: run for interactive shells

fpath=(~/.zsh/functions /usr/local/share/zsh-completions $fpath)

## emacs^[dd
# bindkey -v
## vim<C-S-k>
bindkey -e

# when i've ssh'd into something, stick with vi mode
[[ ! -z $SSH_CLIENT ]] && bindkey -v

## turn on expanded tabcomplete
autoload -U compinit
compinit

## and support some bash completion scripts too
autoload -U bashcompinit
bashcompinit

# `ls /foo/bar<C-w>` only kills 'bar'
autoload -U select-word-style
select-word-style bash

## allows $fg[white], $bg[red], $terminfo[bold]
autoload colors zsh/terminfo
colors

setopt autocd     ## if i type '../somedir', just cd there.
setopt cdablevars ## 'cd foo' can be 'cd $foo' if 'foo' doesn't exist
setopt histignorespace ## omit ' command' from history

## rake foo[bar] without the hassle
# an alternative may be `alias rake='noglob rake'`, if nomatch ends
# up being frustrating
unsetopt nomatch

## history
export SAVEHIST=50000
export HISTSIZE=10000
export HISTFILE=~/.history.zsh
setopt histverify
setopt inc_append_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_expire_dups_first
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt extended_glob

## prompt
# expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

# emacs shell == "dumb"
# disable zsh line editing and let emacs do it
[[ $TERM = "dumb" ]] && unsetopt zle

## generic shell profile setup
function sourcedir () {
  if [ -d "$1" ]; then
    for f in "$1"/*.sh; do source $f; done
  fi
}

sourcedir ~/dotfiles/shell_profile
sourcedir ~/dotfiles/shell_profile/`uname -s`
sourcedir ~/dotfiles/private/shell_profile
sourcedir ~/dotfiles/private/shell_profile/`uname -s`

unfunction sourcedir

# simplistic git status in prompt
pd-git-prompt () {
  local ref=$(git symbolic-ref HEAD 2>/dev/null)
  if [ -z $ref ]; then return; fi
  if [[ -n $(git status -s 2>/dev/null) ]]; then
    local dirty=" δ"
  else
    local dirty=""
  fi
  echo " (${ref#refs/heads/}${dirty})"
}

# equally dumb aws/gcp/... contexts
pd-cloud-contexts () {
  declare -a contexts
  local gcp k8s

  if [[ -n "$AWS_PROFILE" ]]; then
    out=($out "aws/${AWS_PROFILE}")
  fi

  if [[ -f ~/.config/gcloud/active_config ]]; then
    gcp="$(cat ~/.config/gcloud/active_config)"
    if [[ -f ~/.config/gcloud/configurations/"config_${gcp}" ]]; then
      out=($out "gcp/${gcp}")
    fi
  fi

  if [[ -f ~/.kube/config ]]; then
    k8s="$(grep '^current-context' ~/.kube/config | cut -d' ' -f2)"
    if [[ -n "$k8s" ]]; then
      if echo "$k8s" | grep -q '^gke_' >/dev/null 2>&1; then
        k8s="$(echo "$k8s" | cut -d_ -f4-)"
        k8s="${k8s/-cluster/}"
        k8s="${k8s/-env/}"
      fi
      out+=("k8s/${k8s/-cluster/}")
    fi
  fi

  [[ -n "$out" ]] && echo " [$out]"
}

# if this is over ssh, display the hostname to save my brain the effort
if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%~ @ %m » '
else
  export PS1='%~$(pd-git-prompt)$(pd-cloud-contexts) » '
  export RPROMPT='%(?.. %{$fg[red]%}[! %?]%{$fg[white]%})'
fi

# In iTerm, set the tab title
precmd () {
  if [ -z "$INSIDE_EMACS" ]; then
    local tab_label="$(print -P %3~)"
    echo -ne "\e]2;${tab_label}\a" # set window title to full string
    echo -ne "\e]1;${tab_label}\a" # set tab title to rightmost 24 characters
  fi
}
