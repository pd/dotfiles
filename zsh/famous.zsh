## For uploading to my internets site
make-famous () {
  if [[ "$2" = "" ]]; then
    remote_name=$1
  else
    remote_name=$2
  fi
  scp $1 lwweb:/www/internetsfamo.us/httpdocs/$remote_name
  echo http://internetsfamo.us/$remote_name
}
