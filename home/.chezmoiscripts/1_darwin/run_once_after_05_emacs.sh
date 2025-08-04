#!/bin/bash
set -euo pipefail

brew tap d12frosted/emacs-plus
brew install emacs-plus@30

source ~/.profile

echo "install doom emacs"
doom install
doom sync

if [ ! -e "/Applications/Emacs.app" ]; then
    echo "Creating Emacs.app alias in /Applications"
    osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
fi

echo "Creating EmacsClient.app for org-protocol support"

EMACSCLIENT_APP="/Applications/EmacsClient.app"

if [ ! -e "$EMACSCLIENT_APP" ]; then
    # Create EmacsClient.app using Script Editor
    cat > /tmp/emacsclient.applescript << 'EOF'
on emacsclient(input)
    do shell script "/opt/homebrew/bin/emacsclient -n -a \"/Applications/Emacs.app/Contents/MacOS/Emacs\" '" & input & "' && open -a Emacs"
end emacsclient

on open location input
    emacsclient(input)
end open location

on open inputs
    repeat with raw_input in inputs
        set input to POSIX path of raw_input
        emacsclient(input)
    end repeat
end open

on run
    do shell script "/opt/homebrew/bin/emacsclient -n -a \"/Applications/Emacs.app/Contents/MacOS/Emacs\" && open -a Emacs"
end run
EOF

    # Compile AppleScript to app
    osacompile -o "$EMACSCLIENT_APP" /tmp/emacsclient.applescript

    # Edit Info.plist to add org-protocol URL scheme support
    /usr/libexec/PlistBuddy -c "Add :CFBundleURLTypes array" "$EMACSCLIENT_APP/Contents/Info.plist" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :CFBundleURLTypes:0 dict" "$EMACSCLIENT_APP/Contents/Info.plist" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :CFBundleURLTypes:0:CFBundleURLName string 'org-protocol handler'" "$EMACSCLIENT_APP/Contents/Info.plist"
    /usr/libexec/PlistBuddy -c "Add :CFBundleURLTypes:0:CFBundleURLSchemes array" "$EMACSCLIENT_APP/Contents/Info.plist"
    /usr/libexec/PlistBuddy -c "Add :CFBundleURLTypes:0:CFBundleURLSchemes:0 string 'org-protocol'" "$EMACSCLIENT_APP/Contents/Info.plist"

    # Launch EmacsClient.app once to register URL scheme
    echo "Launching EmacsClient.app to register org-protocol URL scheme"
    open "$EMACSCLIENT_APP"
    sleep 2

    # Clean up temporary file
    rm -f /tmp/emacsclient.applescript

    echo "EmacsClient.app created successfully with org-protocol support"
else
    echo "EmacsClient.app already exists, skipping creation"
fi
