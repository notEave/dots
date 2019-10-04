#
# .bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source bash_aliases if they exist
[ -f "$HOME/.bash_aliases" ] && . "$HOME/.bash_aliases"

# Set shell options
shopt -s dotglob
shopt -s autocd

# Set bash prompt
green='\[\033[01;32m\]'
clear='\[\033[00m\]'
blue='\[\033[01;34m\]'
export PS1="$green"'\h'"$clear"':'"$blue"'\W'"$clear"' '"$green"'\u'"$clear"' $ '
export PATH="$PATH:$HOME/Desktop/bin"

# Unified bash history in tmux and other sessions
export PROMPT_COMMAND='history -a;'
shopt -s histappend

export VISUAL='emacsclient --tty'
export EDITOR='emacsclient --tty'

fortune | cowsay
