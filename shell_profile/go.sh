export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

if [ -d /usr/local/opt/go/libexec/bin ]; then
  export PATH=$PATH:/usr/local/opt/go/libexec/bin
fi

alias gt='go test'
