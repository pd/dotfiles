# $OpenBSD: dot.profile,v 1.4 2005/02/16 06:56:57 matthieu Exp $
#
# sh/ksh initialization

set -o vi vi-tabcomplete

PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games
PATH=$HOME/bin:$PATH:.
PATH=$PATH:$PLAN9/bin
export PATH HOME TERM

# PROMPT_COMMAND is bash only.
promptcmd () {
	settitle -t "sh: `whoami`@${hostname}:`dirs`"
}

line () {
	num=$1; shift
	sed -n "${num}{p; q;}" $*
}

case $TERM in
	xterm*|rxvt*)
		PROMPT_COMMAND='promptcmd'
		PS1='$ '
		;;
	screen*)
		PROMPT_COMMAND='promptcmd'
		PS1='\[\033k\033\\\]$ '  # for screen's shelltitle
		;;
	*)
		PS1='$ '
		;;
esac
export PS1

export LANG='en_US.UTF-8'
export CVSROOT='anoncvs@anoncvs1.usa.openbsd.org:/cvs'
export VISUAL='vim'
export BROWSER='links -g'

unset  HISTFILE
unset  RUBYOPT

hostname=${hostname:=`uname -n`}
export hostname=${hostname%.internal}
export p9=$PLAN9
export lcl=/usr/local
export hgweb='http://pd.eggsampler.com/cgi-bin/hgwebdir.cgi'
export PDII_ADDRESS="unix!/tmp/ns.pd.${DISPLAY%.0}/pdii"

alias pkg_add="PKGDIR='/usr/ports/pkg-distfiles6.2' sudo pkg_add -K"
alias ls='ls -F'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias diff='diff -u'
alias cvs='cvs -z9'
alias irb='irb --readline -r irb/completion'

# bash expands the next word
alias sudo='sudo '

export MPD_HOST=localhost
export MPD_PORT=6600
alias pls="echo playlist|nc ${MPD_HOST} ${MPD_PORT}"
