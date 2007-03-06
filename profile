# $OpenBSD: dot.profile,v 1.4 2005/02/16 06:56:57 matthieu Exp $
#
# sh/ksh initialization

unset HISTFILE

PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games
PATH=$HOME/bin:$PATH:.
PATH=$PATH:$PLAN9/bin
export PATH HOME TERM

promptcmd() {
	_whoami=`whoami`
	_uname=`uname -n|sed -e "1s/.internal//"`
	_pwd=`pwd|sed -e "1s/\/home\/pd/~/"`
#	echo -ne "\033]0;${_whoami}@${_uname}:${_pwd}\007"
	settitle "${_whoami}@${_uname}:${_pwd}"
}

case $TERM in
	xterm*|rxvt*)
		PROMPT_COMMAND="promptcmd"
		PS1='$ '
		;;
	screen*)
		PROMPT_COMMAND="promptcmd"
		PS1='\[\033k\033\\\]$ '  # for screen's shelltitle
		;;
	*)
		PS1='$ '
		;;
esac
export PS1

set -o vi vi-tabcomplete

export CVSROOT="anoncvs@anoncvs1.usa.openbsd.org:/cvs"
export VISUAL="vim"
export BROWSER="firefox"
export HGWEB="http://pd.eggsampler.com/cgi-bin/hgwebdir.cgi"
unset RUBYOPT

alias pkg_add="PKGDIR='/usr/ports/pkg-distfiles6.2' sudo pkg_add -K"
alias ls='ls -F'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias diff="diff -u"
alias cvs="cvs -z9"
alias irb="irb --readline -r irb/completion"
alias fcnt="ls -l|head -1|cut -d ' ' -f 2"
