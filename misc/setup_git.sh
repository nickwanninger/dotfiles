#!/usr/bin/env bash

# Show things in columns if you can.
git config --global column.ui auto
# Sort `git branch` output by committerdate
git config --global branch.sort -committerdate
# Launch a daemon that listens for inode events
git config --global core.fsmonitor false
# Assume that I want to push to my remote on the current branch
git config --global push.autosetupremote true
# Use `main`
git config --global init.defaultBranch main
