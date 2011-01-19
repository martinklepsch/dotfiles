# # Include user bin
# export PATH=$PATH:~/bin
#path+=(~/bin)
# recursively add ~/bin
path+=( $HOME/.bin $HOME/.bin/**/*(/N) )
# Prepend homebrew so duplicates are in path
path=( ~/.cabal/bin /usr/local/bin /usr/local/sbin /usr/texbin $path )
path+=( /usr/local/Cellar/android-sdk/r8/platform-tools )

# This loads RVM into a shell session.
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
source "$HOME/.rvm/scripts/rvm"


# History Settings
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

# Aliases
alias v=vim
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

#colors
autoload colors ; colors
export CLICOLOR=1
export LS_COLORS="exfxcxdxbxegedabagacad"
alias ls='ls -G -F'
