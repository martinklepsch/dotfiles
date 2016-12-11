function notify-me --on-event fish_postexec
  set -l last_cmd $argv
  if test -n $last_cmd
    set -l focused (iterm-focused)
    # echo "iterm-focused result:" $focused
    if test $focused = "not-focused"
      # echo "status" $status
      if test $status -gt 0
        echo "sending failed notification" $status
        terminal-notifier -message "failed" -title $last_cmd -activate com.googlecode.iterm2
      else
        terminal-notifier -message "done" -title $last_cmd -activate com.googlecode.iterm2
      end
    else
    end
  end
end