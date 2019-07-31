#
# .bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source bash_aliases if they exist
[ -f '.bash_aliases' ] && source '.bash_aliases'

# Set bash prompt
export PS1='\[\033[01;32m\]\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\] \[\033[01;32m\]\u\[\033[00m\] $ '

# Write OSX-style last login line
LAST_LOG=$(last $USER -F --limit 2 hoki | sed '2q;d')
echo "Last login: $(echo $LAST_LOG | awk '{print $4" "$5" "$6" "$7}') on \
$(echo $LAST_LOG | awk '{print $2}')"
