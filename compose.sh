#!/bin/bash

set -e

usage () {
    cat<<EOF
Usage: $0 [-b]

  -b   build meccg-node image
EOF
}

while getopts ':b' o; do
    case "${o}" in
        b)
            BUILD=true
            ;;
        *)
            usage
            ;;
    esac
done

if [[ -n ${BUILD} ]]; then
    docker build -t meccg-node .
fi

docker-compose up npm-install
docker-compose up -d mongodb
docker-compose up -d stylus-css
docker-compose up lein-meccg lein-cljs
