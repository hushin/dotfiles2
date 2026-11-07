' Start the browse relay at logon, hidden.
'
' Windows owns the relay and the Chrome extension pairing. WSL reaches the same
' relay over mirrored networking (localhost:19988), so it must be running before
' an agent uses browse. Does nothing when browse is not installed yet.
Set sh = CreateObject("WScript.Shell")
exe = sh.ExpandEnvironmentStrings("%USERPROFILE%") & "\.local\bin\browse.exe"
If CreateObject("Scripting.FileSystemObject").FileExists(exe) Then
    sh.Run """" & exe & """ serve", 0, False
End If
