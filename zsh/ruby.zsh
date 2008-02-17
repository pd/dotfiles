## way too much to type
gems="/opt/local/lib/ruby/gems/1.8/gems"

## testing
alias att='autotest'
alias rbst='ruby stories/stories/all.rb'

## shortcuts to most common rails commands
alias sse='./script/server'
alias sst='./script/story'
alias sc='./script/console'
alias gs='gem server'

## works for both rails and merb-core
sg () {
  if [ -f './script/generate' ]; then
    ruby ./script/generate $*
  else
    merb-gen $*
  fi
}

mt () {
  [ "$PWD" = "$HOME" ] && echo "not in $HOME." && return
  mate .
}
