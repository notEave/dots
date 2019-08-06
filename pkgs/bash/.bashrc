#
# .bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source bash_aliases if they exist
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"

# Set shell options
shopt -s dotglob
shopt -s autocd

# Set bash prompt
GREEN='\[\033[01;32m\]'
CLEAR='\[\033[00m\]'
BLUE='\[\033[01;34m\]'
export PS1="$GREEN"'\h'"$CLEAR"':'"$BLUE"'\W'"$CLEAR"' '"$GREEN"'\u'"$CLEAR"' $ '
export PATH="$PATH:$HOME/Desktop/bin"

export VISUAL='emacsclient --tty'
export EDITOR='emacsclient --tty'

# Write OSX-style last login line
LOGIN=$(last -F --limit 2 "$USER" | sed '2q;d')
echo "Last login: $(echo "$LOGIN" | awk '{print $4" "$5" "$6" "$7}') on \
$(echo "$LOGIN" | awk '{print $2}')"
