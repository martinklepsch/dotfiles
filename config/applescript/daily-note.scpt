on run
	tell application "Notes"
		set todayDate to do shell script "date -v -Mon '+%Y-%m-%d'"
		set dailyNote to text 1 thru 10 of todayDate
		
		-- display dialog "my variable: " & todayDate
		
		set dateOut to do shell script "date '+%A (Week %U)'"
		set noteBody to "<h1>" & dateOut & "</h1>"
		
		if (exists note dailyNote) then

		        -- display dialog "opening existing note: " & todayDate
			show note dailyNote
		else
		        -- display dialog "else: " & todayDate
			make new note at folder "in the moment notes" with properties {name:dailyNote, body:noteBody}
			show note dailyNote
		end if
	end tell
end run
