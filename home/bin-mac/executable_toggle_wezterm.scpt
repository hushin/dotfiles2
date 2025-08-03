-- WezTerm Toggle Script

tell application "System Events"
    set appList to name of application processes
    if appList contains "WezTerm" then
        tell application process "WezTerm"
            if visible then
                set visible to false
            else
                set visible to true
                tell application "WezTerm" to activate
            end if
        end tell
    else
        tell application "WezTerm" to activate
    end if
end tell
