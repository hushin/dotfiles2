@echo off
rem ksec -- cmd.exe / PowerShell entry point.
rem The CLI is implemented once, in bash (~/bin/ksec); this only forwards to it.
rem Keep this file ASCII-only with CRLF: cmd.exe reads batch files in the OEM codepage.
setlocal
set "KSEC_BASH="
for %%B in ("%ProgramFiles%\Git\bin\bash.exe" "%LOCALAPPDATA%\Programs\Git\bin\bash.exe") do if exist "%%~B" set "KSEC_BASH=%%~B"
if not defined KSEC_BASH (
  echo ksec: Git bash.exe not found 1>&2
  exit /b 1
)
if not exist "%USERPROFILE%\bin\ksec" (
  echo ksec: %USERPROFILE%\bin\ksec not found, run chezmoi apply 1>&2
  exit /b 1
)
"%KSEC_BASH%" --noprofile --norc "%USERPROFILE%\bin\ksec" %*
exit /b %ERRORLEVEL%
