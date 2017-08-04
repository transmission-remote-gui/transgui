#!/bin/bash

error() { >&2 echo -e "\033[1;31m$*\033[m"; exit 1; }

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
    echo -e "\033[36mUploading $1 ...\033[m";
    if ! curl -F "file=@$1" "https://file.io/?expires=8d" > upload.log; then
        error "$1 upload failed!!!"
    fi
    echo -e "\033[32m$1 uploaded with success!!!\033[m";
    echo -e "\033[32mDownload url:\033[m $(jq -r .link upload.log 2>/dev/null || awk -F'"' '{print $10}' upload.log) \033[33m(expires in 1 week)\033[m";
    rm upload.log
fi
