
# 現在のセッションの PATH 情報を更新するための関数
function Update-PathVariable {
  $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") +
  ";" +
  [System.Environment]::GetEnvironmentVariable("Path", "User")
}

Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None

$env:CARAPACE_BRIDGES = "zsh,fish,bash,inshellisense" # optional
Set-PSReadLineOption -Colors @{ "Selection" = "`e[7m" }
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
carapace _carapace | Out-String | Invoke-Expression

Invoke-Expression (& { (zoxide init powershell | Out-String) })
