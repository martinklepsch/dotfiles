# Usefult aliases {{{

alias c 'clear'
alias hl 'less -R'
function paththis
  set -xg PATH (pwd) $PATH
end

function f
  fzf > $TMPDIR/fzf.result; and em (cat $TMPDIR/fzf.result)
end 

# Mac OS X helper utilities
alias hide-desktop 'defaults write com.apple.finder CreateDesktop false; killall Finder'
alias show-desktop 'defaults write com.apple.finder CreateDesktop true; killall Finder'
alias better-dns 'sudo networksetup -setdnsservers Wi-Fi 8.8.8.8 8.8.4.4 208.67.222.222 208.67.220.220'
alias awdl 'sudo ifconfig awdl0' # See: https://medium.com/@mariociabarra/wifried-ios-8-wifi-performance-issues-3029a164ce94
alias sprunge 'curl -F \'sprunge=<-\' http://sprunge.us'

# Setup Vi Mode
fish_vi_mode
function my_vi_bindings
  fish_vi_key_bindings
  bind -M insert -m default kj backward-char force-repaint
end
set -g fish_key_bindings my_vi_bindings

# shorten often used commands
alias g 'git'

alias ef 'em ~/.config/fish/config.fish'
alias ea 'em ~/.config/awesome/rc.lua'
alias eg 'em ~/.gitconfig'
alias et 'em ~/.tmux.conf'
alias ev 'em ~/.vim/vimrc'
alias ee 'em ~/.emacs.d/init.el'
alias eo 'em ~/Dropbox/org/testing.org'
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

alias ovd 'overcast digitalocean'
alias ovr 'overcast run'

set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8
# USER: set important paths here to put at the front of $PATH if you want to override system-wide settings
# set -x NIX_PATH nixpkgs=$HOME/code/nixpkgs
# set -x PATH $HOME/.nix-profile/bin $PATH
set -x PATH $HOME/.bin $PATH
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

# Java Stuff
# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)
# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8.0_60)
set -x BOOT_JAVA_COMMAND /Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/bin/java
set -x BOOT_JVM_OPTIONS "-Xmx2g -client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xverify:none"

set -g -x fish_greeting ''
set -g -x EDITOR 'vim'
set -g -x XDG_CONFIG_HOME ~/.config
set -g -x COMMAND_MODE unix2003
set -g -x RUBYOPT rubygems
set -g -x DOCKER_HOST tcp://localhost:4243


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
set red (set_color red)
set gray (set_color -o black)

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
   curl -L $argv[1] | git am
end

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

  printf '%s ' (__fish_git_prompt)

  set_color normal
end

if [ -f "/usr/local/Cellar/autojump/22.3.0/share/autojump/autojump.fish" ]
  . "/usr/local/Cellar/autojump/22.3.0/share/autojump/autojump.fish"
end

# OPAM configuration
set -gx PATH "$HOME/.opam/system/bin" $PATH;
set -gx OCAML_TOPLEVEL_PATH "$HOME/.opam/system/lib/toplevel";
set -gx PERL5LIB "$HOME/.opam/system/lib/perl5:$PERL5LIB";
set -gx MANPATH $MANPATH "$HOME/.opam/system/man";
set -gx OPAMUTF8MSGS "1";
set -gx CAML_LD_LIBRARY_PATH "$HOME/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs";