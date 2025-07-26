# Microsoft Teamsアンインストールスクリプト

Write-Host "=======================================================" -ForegroundColor Green
Write-Host "Microsoft Teams アンインストールスクリプト" -ForegroundColor Green
Write-Host "=======================================================" -ForegroundColor Green
Write-Host ""

# エラーハンドリング用関数
function Write-Status {
    param(
        [string]$Message,
        [string]$Status = "INFO"
    )

    switch ($Status) {
        "SUCCESS" { Write-Host "✓ $Message" -ForegroundColor Green }
        "ERROR" { Write-Host "✗ $Message" -ForegroundColor Red }
        "WARNING" { Write-Host "⚠ $Message" -ForegroundColor Yellow }
        "INFO" { Write-Host "ℹ $Message" -ForegroundColor Cyan }
    }
}

# 1. Teamsプロセスの終了
Write-Status "Microsoft Teamsプロセスの終了中..."
try {
    $teamsProcesses = @("ms-teams", "Teams", "TeamsUpdater", "SquirrelSetup")

    foreach ($process in $teamsProcesses) {
        Get-Process -Name $process -ErrorAction SilentlyContinue | Stop-Process -Force
    }
    Write-Status "Microsoft Teamsプロセスを終了しました" "SUCCESS"
}
catch {
    Write-Status "Microsoft Teamsプロセスの終了中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}

# 2. 現在のユーザーのTeamsをアンインストール
Write-Status "現在のユーザーのMicrosoft Teamsをアンインストール中..."

# Wingetを使用してアンインストール（Microsoft.Teams）
try {
    Write-Status "Microsoft.Teamsをアンインストール中..."
    winget uninstall Microsoft.Teams --silent
}
catch {
    Write-Status "Microsoft.Teamsのアンインストール中にエラーが発生しました" "WARNING"
}

# 3. Microsoft Teams Add-in をアンインストール
try {
    Write-Status "Microsoft Teams Add-in をアンインストール中..."
    winget uninstall "Microsoft Teams" --silent
}
catch {
    Write-Status "Microsoft Teams Add-in のアンインストール中にエラーが発生しました" "WARNING"
}

# 4. Teamsのスタートアップエントリを削除
Write-Status "Teamsのスタートアップエントリを削除中..."
try {
    $runPath = "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"

    $startupEntries = @("Teams", "com.squirrel.Teams.Teams")
    foreach ($entry in $startupEntries) {
        if (Get-ItemProperty -Path $runPath -Name $entry -ErrorAction SilentlyContinue) {
            Remove-ItemProperty -Path $runPath -Name $entry -ErrorAction SilentlyContinue
        }
    }
    Write-Status "スタートアップエントリを削除しました" "SUCCESS"
}
catch {
    Write-Status "スタートアップエントリの削除に失敗しました: $($_.Exception.Message)" "WARNING"
}

# 5. Teamsのフォルダーとキャッシュのクリーンアップ
Write-Status "Teamsのフォルダーとキャッシュのクリーンアップ中..."
try {
    $userProfile = $env:USERPROFILE
    $teamsPaths = @(
        (Join-Path $userProfile "AppData\Local\Microsoft\TeamsMeetingAdd-in"),
        (Join-Path $userProfile "AppData\Local\Microsoft\TeamsMeetingAddinMsis"),
        (Join-Path $userProfile "AppData\Roaming\Microsoft\Teams"),
        (Join-Path $userProfile "AppData\Local\SquirrelTemp")
    )

    foreach ($path in $teamsPaths) {
        if (Test-Path $path) {
            try {
                Remove-Item -Path $path -Recurse -Force -ErrorAction SilentlyContinue
                Write-Status "Teamsフォルダーを削除しました: $path" "SUCCESS"
            }
            catch {
                Write-Status "Teamsフォルダーの削除に失敗しました: $path" "WARNING"
            }
        }
    }
}
catch {
    Write-Status "Teamsフォルダーのクリーンアップ中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}

# 6. レジストリエントリのクリーンアップ
Write-Status "Teamsレジストリエントリのクリーンアップ中..."
try {
    $registryPaths = @(
        "HKCU:\SOFTWARE\Microsoft\Teams",
        "HKCU:\SOFTWARE\Classes\Local Settings\Software\Microsoft\Windows\CurrentVersion\AppModel\Repository\Packages\Microsoft*Teams*"
    )

    foreach ($path in $registryPaths) {
        if (Test-Path $path) {
            try {
                Remove-Item -Path $path -Recurse -Force -ErrorAction SilentlyContinue
                Write-Status "レジストリエントリを削除しました: $path" "SUCCESS"
            }
            catch {
                Write-Status "レジストリエントリの削除に失敗しました: $path" "WARNING"
            }
        }
    }
}
catch {
    Write-Status "レジストリエントリのクリーンアップ中にエラーが発生しました: $($_.Exception.Message)" "WARNING"
}

Write-Host "スクリプトの実行が完了しました。" -ForegroundColor Green
