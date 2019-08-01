#!/bin/bash

#
# Symlink dotfiles to system directories
#
# In case file is inside a directory, the directory needs to exists
# for the file to be symlinked
#

! command -v readlink > /dev/null && exit 1
! command -v shopt > /dev/null && exit 1
! command -v xargs > /dev/null && exit 1

shopt -s dotglob
cp -ais --backup=t $(readlink -f "$0" | xargs dirname)/home/* "$HOME/"
