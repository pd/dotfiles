## ruby/rails misc.
alias sse='./script/server'
alias sst='./script/story'
alias sc='./script/console'
alias sg='./script/generate'
alias att='autotest'
alias rbst='ruby stories/all.rb'
alias gs='gem server'

function mt {
  [ "$PWD" = "$HOME" ] && echo "not in $HOME." && return
  mate .
}

## way too much to type
gems="/opt/local/lib/ruby/gems/1.8/gems"
