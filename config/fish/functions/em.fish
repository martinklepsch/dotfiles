function em
  if test -z (pgrep Emacs)
    env LANG=C open -a Emacs $argv
    echo "Emacs: starting up"
    return
  end

  echo "Emacs: running"
  alias client "/usr/local/bin/emacsclient"
  set -l uid    (id -u)
  set -l socket $TMPDIR/emacs$uid/server

  if not test -e $socket
    socket (getconf DARWIN_USER_TEMP_DIR)emacs"$uid"/server
  end

  client -n -s $socket $argv
end
