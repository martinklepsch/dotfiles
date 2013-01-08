# Useful aliases {{{

alias c 'clear'
alias hl 'less -R'
alias paththis 'set PATH (pwd) $PATH'

# shorten often used commands
alias g 'git'

alias ef 'vim ~/.config/fish/config.fish'
alias ea 'vim ~/.config/awesome/rc.lua'
alias eg 'vim ~/.gitconfig'
alias et 'vim ~/.tmux.conf'
alias ev 'vim ~/.vim/vimrc'
alias be 'bundle exec'

# }}}
# Environment variables {{{

set PATH "/usr/local/sbin" $PATH
set PATH "/usr/sbin" $PATH
set PATH "/sbin" $PATH
# Ruby Stuff
set PATH "$HOME/.rbenv/shims" $PATH
set PATH "$HOME/.rbenv/bin" $PATH
# Perl Stuff
set PATH "/usr/bin/vendor_perl" $PATH
set PATH "/usr/bin/core_perl" $PATH


#[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] &&
# . $HOME/.tmuxinator/scripts/tmuxinator



set -g -x fish_greeting ''
set -g -x EDITOR vim
set -g -x XDG_CONFIG_HOME ~/.config
set -g -x COMMAND_MODE unix2003
set -g -x RUBYOPT rubygems
# TODO MKL Actual thing here
# set -g -x CLASSPATH "$CLASSPATH:/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar"


alias ...   'cd ../..'
alias ....  'cd ../../..'
alias ..... 'cd ../../../..'

alias md 'mkdir -p'

alias l1 'tree --dirsfirst -ChFL 1'
alias l2 'tree --dirsfirst -ChFL 2'
alias l3 'tree --dirsfirst -ChFL 3'

alias ll1 'tree --dirsfirst -ChFupDaL 1'
alias ll2 'tree --dirsfirst -ChFupDaL 2'
alias ll3 'tree --dirsfirst -ChFupDaL 3'

alias l  'l1'
alias ll 'll1'

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set gray (set_color -o black)

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
# set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
#set __fish_git_prompt_color_upstream green

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'


function fish_prompt
  set last_status $status

  # echo

  # set_color magenta
  # printf '%s' (whoami)
  # set_color normal
  # printf ' at '

  # set_color yellow
  # printf '%s' (hostname|cut -d . -f 1)
  # set_color normal
  # printf ' in '

  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  printf '%s ' (__fish_git_prompt)

  set_color normal
end


