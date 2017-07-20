#!/bin/bash

error() { >&2 echo -e "\e[1;31m$*\e[m"; exit 1; }

rm -rf upload.log

if [ "$#" = 0 ]; then
    error "You should give me the filename of the file you want to upload"
elif [ ! "$#" = 1 ]; then
    error "I only accept one file per time"
elif [ ! -e "$1" ]; then
    error "'$1' not exist"
elif [ ! -r "$1" ]; then
    error "'$1' not readable"
else
    echo -e "\e[36mUploading $1 ...\e[m";
    if ! curl -F "file=@$1" "https://file.io/?expires=8d" > upload.log; then
        error "$1 upload failed!!!"
    fi
    echo -e "\e[32m$1 uploaded with success!!!\e[m";
    echo -e "\e[32mDownload url:\e[m $(jq -r .link upload.log) \e[33m(expires in 1 week)\e[m";
    rm upload.log
fi
