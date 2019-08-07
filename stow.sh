#!/bin/bash

#
# Bash frontend for stowing, restowing and removing dotfile packages
#

# Program dependency check
command -v readlink > /dev/null || exit 1
command -v stow > /dev/null ||  exit 1

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
    select PKG in $(echo pkgs/* | xargs basename -a; printf 'ALL\nQUIT')
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

function act_all {
    for PACK in $(echo pkgs/* | xargs basename -a)
    do
        stow -v -d pkgs -t ~ -"${MODE}" "${PACK}"
    done
}

 
while 'true'
do
    select_action
    select_package
    if [ "${PKG}" = 'ALL' ]
    then
        act_all
    else
        echo "${PKG}"
        echo pkgs
        stow -v -d pkgs -t ~ -"${MODE}" "${PKG}"
    fi
    echo
done
