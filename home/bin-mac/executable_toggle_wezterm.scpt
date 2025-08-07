-- WezTerm Toggle Script

tell application "System Events"
    set appList to name of application processes
    if appList contains "wezterm-gui" then
        tell application process "wezterm-gui"
            if visible then
                -- WezTermが見えている場合、最前面かどうかチェック
                if frontmost then
                    -- 最前面なら隠す
                    set visible to false
                else
                    -- 最前面でなければ最前面にする
                    tell application "WezTerm" to activate
                end if
            else
                -- 隠れているなら表示して最前面にする
                set visible to true
                tell application "WezTerm" to activate
            end if
        end tell
    else
        tell application "WezTerm" to activate
    end if
end tell
