#!/bin/bash

#
# Bash frontend for stowing, restowing and removing dotfile packages
#

# Program dependency check
command -v readlink || exit 1
command -v stow || exit 1

# Exit if cd fails
cd "$(readlink -f "$0" | xargs dirname)" || exit 1

function select_action {
    echo 'Select action:'
    select ACTION in $(printf 'stow\nrestow\ndelete\nQUIT')
    do
        case "${ACTION}"
        in
            stow)
                MODE='S'
                break
                ;;
            restow)
                MODE='R'
                break
                ;;
            delete)
                MODE='D'
                break
                ;;
            QUIT)
                echo You selected QUIT
                exit 0
                break
                ;;
            *)
                ;;
        esac
    done
    echo You selected "${ACTION}"
    echo
}

function select_package {
    echo 'Select package:'
    select PKG in $(echo pkgs/* | xargs basename -a; echo QUIT)
    do
        case "${PKG}"
        in
            QUIT)
                echo You selected QUIT
                exit 0
                ;;
            *)
                break
                ;;
        esac
    done
    echo You selected "${PKG}"
    echo
}

while 'true'
do
    select_action
    select_package
    stow -v -d pkgs -t ~ -"${MODE}" "${PKG}"
    echo
done
