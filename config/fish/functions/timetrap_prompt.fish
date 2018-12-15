function timetrap_prompt
    # A custom prompt element to show the currently checked in timetrap sheet and the already
    # passed time. https://github.com/samg/timetrap/
    set -l timetrap_db "$HOME/Documents/Timetracking/timetrap.sqlite.db"
    set -l start (sqlite3 $timetrap_db "select start from entries where end is null;" | string sub -l 19)
    set -l current_sheet (sqlite3 $timetrap_db "select value from meta where key = 'current_sheet';")

    if [ -n "$start" ]
        # on non Mac OS this might need to look more like this:
        # date --date="Oct 1 09:00:00 BST 2009" +%s
        set -l start_s (date -jf '%Y-%m-%d %H:%M:%S' $start '+%s')
        set -l seconds_checked_in (math (date +%s) - $start_s)
        set -l hours (math "$seconds_checked_in / (60 * 60)")
        set -l minutes (math "$seconds_checked_in % (60 * 60) / 60")
        printf " ($yellow%s$normal: $green%02i:%02i$normal)" $current_sheet $hours $minutes
    else
        printf " ($red%s$normal)" "off" #$current_sheet
    end
end
