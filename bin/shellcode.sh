#!/bin/bash

if [ "$1" != "" ]; then
	nasm -f bin "$1" -o /dev/stdout | hexdump -v -e '"\\" "\x" 1/1 "%x"'
fi
