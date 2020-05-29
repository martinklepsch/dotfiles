# Useful aliases {{{

alias c 'clear'
alias hl 'less -R'
alias d 'sudo docker'
alias ia 'open -a "iA Writer"'
alias cdicloud 'cd /Users/martinklepsch/Library/Mobile\ Documents/com~apple~CloudDocs/'
function paththis
  set -xg PATH (pwd) $PATH
end

function f
  if test -d .git
    git ls-files | uniq | fzf | xargs $EDITOR
  else
    fzf | xargs $EDITOR
  end
end

function md5-suffix
  set -l path $argv[1]
  set -l suffix $argv[2]
  mv $path (basename $path $suffix)-(md5 -q $path | cut -c1-5)$suffix
end

# Mac OS X helper utilities
alias hide-desktop 'defaults write com.apple.finder CreateDesktop false; killall Finder'
alias show-desktop 'defaults write com.apple.finder CreateDesktop true; killall Finder'
alias show-hidden 'defaults write com.apple.finder AppleShowAllFiles YES; killall Finder'
alias hide-hidden 'defaults write com.apple.finder AppleShowAllFiles NO; killall Finder'
alias better-dns 'sudo networksetup -setdnsservers Wi-Fi 8.8.8.8 8.8.4.4 208.67.222.222 208.67.220.220'
alias awdl 'sudo ifconfig awdl0' # See: https://medium.com/@mariociabarra/wifried-ios-8-wifi-performance-issues-3029a164ce94
alias sprunge 'curl -F \'sprunge=<-\' http://sprunge.us'

# Setup Vi Mode
# function fish_user_key_bindings
# 	bind -M insert -m default kj backward-char force-repaint
# end
function my_vi_bindings
  fish_vi_key_bindings
  bind -M insert -m default kj backward-char force-repaint
end
set -g fish_key_bindings my_vi_bindings

# shorten often used commands
alias g 'git'

alias ec 'pushd ~/etc; git ls-files | fzf | xargs $EDITOR; popd'
alias ef '$EDITOR ~/.config/fish/config.fish'
alias ea '$EDITOR ~/.config/awesome/rc.lua'
alias eg '$EDITOR ~/.gitconfig'
alias et '$EDITOR ~/.tmux.conf'
alias ev '$EDITOR ~/.config/nvim/init.vim'
alias ee '$EDITOR ~/.emacs.d/init.el'
alias eo '$EDITOR ~/Dropbox/org/testing.org'
alias be 'bundle exec'

alias tma 'tmux attach -t'
alias tml 'tmux list-sessions'
alias tmn 'tmux new -s'
alias tmn. 'tmux new -s (basename $PWD)'

alias boot-local '$HOME/code/boot/bin/boot.sh'
function boot23
  set -xg BOOT_VERSION "2.3.0"
  eval boot $argv
end
function boot23s
  set -xg BOOT_VERSION "2.3.1-SNAPSHOT";
  eval /Users/martin/code/boot/bin/boot.sh $argv
end

set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8
# USER: set important paths here to put at the front of $PATH if you want to override system-wide settings
# set -x NIX_PATH nixpkgs=$HOME/code/nixpkgs
# set -x PATH $HOME/.nix-profile/bin $PATH
set -x PATH $HOME/.bin $PATH
set -x PATH $HOME/code/02-oss/Fennel $PATH
set -x PATH $HOME/code/03-personal/tlog/_build $PATH
# set -x ANSIBLE_HOME $HOME/code/ansible
# set -x PATH $ANSIBLE_HOME/bin $PATH
# set -x PYTHONPATH $ANSIBLE_HOME/lib $PYTHONPATH
# set PATH "/usr/local/sbin" $PATH
# set PATH "/usr/sbin" $PATH
# set PATH "/sbin" $PATH
# Ruby Stuff
# set PATH $HOME/.rbenv/bin $PATH
# set PATH $HOME/.rbenv/shims $PATH
# rbenv rehash >/dev/null ^&1

# Perl Stuff
# set PATH "/usr/bin/vendor_perl" $PATH
# set PATH "/usr/bin/core_perl" $PATH

# Node Stuff
status --is-interactive; and source (nodenv init -|psub)

# Java Stuff
# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)
# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8.0_60)
# set -x BOOT_JAVA_COMMAND /Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/bin/java

alias j7 'set -gx JAVA_HOME (/usr/libexec/java_home -v 1.7.0_80)'
alias j8 'set -gx JAVA_HOME (/usr/libexec/java_home -v 1.8.0_181)'
alias j9 'set -gx JAVA_HOME (/usr/libexec/java_home -v 9.0.1)'
alias j10 'set -gx JAVA_HOME (/usr/libexec/java_home -v 10.0.1)'
alias j11 'set -gx JAVA_HOME (/usr/libexec/java_home -v 11.0.6)'

set -gx GOPATH ~/code/08-go
set -gx PATH $GOPATH/bin $PATH

set -x BOOT_JVM_OPTIONS "-Xmx2g -client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xverify:none"

set -gx fish_greeting ''
set -gx EDITOR 'nvim'
set -gx XDG_CONFIG_HOME ~/.config
set -gx COMMAND_MODE unix2003
set -gx RUBYOPT rubygems

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

alias lua 'rlwrap lua'
alias fennel 'rlwrap fennel'

if status --is-interactive
  set normal (set_color normal)
  set magenta (set_color magenta)
  set yellow (set_color yellow)
  set green (set_color green)
  set red (set_color red)
  set gray (set_color -o black)
end

# Fish git prompt
# set __fish_git_prompt_showdirtystate 'yes'
# set __fish_git_prompt_showstashstate 'yes'
# set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'

function applypr
   set f (mktemp)
   echo "Downloading patch to $f"
   curl -sL $argv[1] -o $f
   git am -3 $f
end

if status --is-interactive
  function fish_prompt
    set last_status $status

    switch $fish_bind_mode
      case "insert"
        set_color $fish_color_cwd
      case "default"
        set_color red
    end

    printf '%s' (prompt_pwd)
    set_color normal

    # I once accidentially deleted the file defining tlog_prompt
    # https://twitter.com/martinklepsch/status/1266455561400397831
    # printf '%s' (tlog_prompt)

    printf '%s ' (__fish_git_prompt)

    set_color normal
  end
end

# OPAM configuration
# if test (type opam)
#  opam config env | source
# end

if test (type pass)
  set -gx PASSWORD_STORE_DIR "$HOME/pass"
end

if test (type direnv)
  direnv hook fish | source
end
