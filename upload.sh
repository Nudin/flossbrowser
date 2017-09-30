#!/bin/bash

set -e

cd src

function show_help() {
  echo -e "./upload -[bfdrh?]\n"
  echo -e "\t-b\tUpload backend"
  echo -e "\t-f\tUpload frontend"
  echo -e "\t-d\tUpload database"
  echo -e "\t-r\tCreate database"
  echo -e "\t-h -?\tShow this help"
}

if [[ $# -eq 0 ]]; then
  show_help
  exit
fi

backend=false
frontend=false
database=false
run=false
while getopts "h?bfdr" opt; do
    case "$opt" in
    h|\?)
        show_help
        exit 0
        ;;
    b)  backend=true
        ;;
    f)  frontend=true
        ;;
    d)  database=true
        ;;
    r)  run=true
        ;;
    esac
done
shift $((OPTIND-1))

server="login.tools.wmflabs.org"
path="/data/project/flossbrowser/"

if $frontend; then
  make Browser
  if [[ -e "Browser.gz" ]] ; then
    rm Browser.gz
  fi
  gzip Browser
  scp Browser.gz ${server}:${path}
  rm Browser.gz
fi

if $backend; then
  make CreateDB
  scp CreateDB ${server}:${path}
fi

if $run; then
  ./CreateDB
fi

if $database; then
  scp flossbrowser.sqlite ${server}:${path}
fi
