#!/bin/bash

cd /Users/nick/dotfiles

if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
  echo "dirty"
else
  echo "clean"
fi

