# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="mklappstuhl"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode vagrant zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Aliases
alias r="ranger"
alias vawesome="vim ~/.config/awesome/rc.lua"
alias alert="vlc ~/Music/Parov\ Stelar/Parov_Stelar-Coco/210-parov_stelar-monster_\(original_version\).mp3 &"
alias rake="noglob rake"
alias movie="xset s off -dpms"

# SSH Key Management
eval `keychain --eval github_mklappstuhl cct_mklepsch`

# Include user bin recursively
path+=( $HOME/.bin $HOME/.bin/**/*(/N) )
# Include rbenv binaries
path+=( $HOME/.rbenv/bin )
eval "$(rbenv init -)"
