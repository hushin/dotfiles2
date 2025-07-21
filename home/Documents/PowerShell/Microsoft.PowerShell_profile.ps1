# $PROFILE.CurrentUserCurrentHost

# 現在のセッションの PATH 情報を更新するための関数
function Update-PathVariable {
  $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") +
  ";" +
  [System.Environment]::GetEnvironmentVariable("Path", "User")
}

# miseのshimパスを追加
$shimPath = "$env:USERPROFILE\AppData\Local\mise\shims"
$currentPath = [Environment]::GetEnvironmentVariable('Path', 'User')
$newPath = $currentPath + ";" + $shimPath
[Environment]::SetEnvironmentVariable('Path', $newPath, 'User')

Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None

# carapace (補完ツール) の設定
$env:CARAPACE_BRIDGES = "zsh,fish,bash,inshellisense" # optional
Set-PSReadLineOption -Colors @{ "Selection" = "`e[7m" }
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
carapace _carapace | Out-String | Invoke-Expression

# zoxide (cdの改良) の初期化
Invoke-Expression (& { (zoxide init powershell | Out-String) })
# Starship の初期化 (最後に追加)
Invoke-Expression (&starship init powershell)
