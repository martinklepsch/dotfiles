# # Include user bin
# export PATH=$PATH:~/bin
#path+=(~/bin)
# recursively add ~/bin
path+=( $HOME/.bin $HOME/.bin/**/*(/N) )
# Prepend homebrew so duplicates are in path
path=( ~/.cabal/bin /usr/local/bin /usr/local/sbin /usr/texbin $path )

# History Settings
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

#TERM for TMUX
if [[ -n $TMUX ]]; then
	TERM=screen-256color
fi

#if [[ -z $TMUX ]] ; then 
	#tmux attach; 
#fi

#nohup ~/.zsh/scripts/pbcopy_server.zsh &

source ~/.zsh/custom/z-zsh/z.sh
function precmd () {
	z --add "$(pwd -P)"
}

# load prompts
autoload -U promptinit
promptinit
prompt adam2

# vi-mode
bindkey -v
# display TODOs quickly
alias todos="grep -r 'TODO' ."
# display umlauts etc.
#setopt combining
setopt combining_chars
