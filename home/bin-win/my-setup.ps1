# Interactive PowerShell Subcommand Runner
# Usage: .\my-setup.ps1 [subcommand] [arguments...]

param(
    [Parameter(Position = 0)]
    [string]$SubCommand,

    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Arguments
)

# スクリプトのディレクトリを取得
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$SubCommandDir = Join-Path $ScriptDir "setup-scripts"

# 利用可能なサブコマンドを取得する関数
function Get-AvailableSubCommands {
    if (Test-Path $SubCommandDir) {
        $scripts = Get-ChildItem -Path $SubCommandDir -Filter "*.ps1" | ForEach-Object {
            $_.BaseName
        }
        return $scripts
    }
    return @()
}

# ヘルプを表示する関数
function Show-Help {
    Write-Host "PowerShell サブコマンドランナー (fzf対応)" -ForegroundColor Green
    Write-Host "使用方法: .\my-setup.ps1 [サブコマンド] [引数...]" -ForegroundColor Yellow
    Write-Host "デフォルト: fzf対話モード" -ForegroundColor Yellow
    Write-Host ""

    $availableCommands = Get-AvailableSubCommands
    if ($availableCommands.Count -gt 0) {
        Write-Host "利用可能なサブコマンド:" -ForegroundColor Cyan
        foreach ($cmd in $availableCommands) {
            Write-Host "  $cmd" -ForegroundColor White
        }
    }
    else {
        Write-Host "setup-scripts/ ディレクトリにサブコマンドが見つかりません。" -ForegroundColor Red
    }
    Write-Host ""
    Write-Host "例:"
    Write-Host "  .\my-setup.ps1                    # fzf対話モード（デフォルト）"
    Write-Host "  .\my-setup.ps1 sub-command arg1   # 直接実行"
    Write-Host "  .\my-setup.ps1 help               # ヘルプ表示"
}

# サブコマンドを実行する関数
function Invoke-SubCommand {
    param(
        [string]$CommandName,
        [string[]]$Arguments
    )

    $scriptPath = Join-Path $SubCommandDir "$CommandName.ps1"

    if (Test-Path $scriptPath) {
        Write-Host "実行中: $CommandName" -ForegroundColor Green
        if ($Args.Count -gt 0) {
            Write-Host "引数: $($Args -join ' ')" -ForegroundColor Gray
        }
        Write-Host "----------------------------------------" -ForegroundColor Gray

        try {
            # サブコマンドスクリプトを実行
            & $scriptPath @Arguments
            Write-Host "----------------------------------------" -ForegroundColor Gray
            Write-Host "完了: $CommandName" -ForegroundColor Green
        }
        catch {
            Write-Host "エラー: $($_.Exception.Message)" -ForegroundColor Red
        }
    }
    else {
        Write-Host "エラー: サブコマンド '$CommandName' が見つかりません。" -ForegroundColor Red
        Write-Host "パス: $scriptPath" -ForegroundColor Gray
    }
}

# fzfでコマンドを選択する関数
function Select-CommandWithFzf {
    $availableCommands = Get-AvailableSubCommands

    if ($availableCommands.Count -eq 0) {
        Write-Host "setup-scripts/ ディレクトリにサブコマンドが見つかりません。" -ForegroundColor Red
        return $null
    }

    # 特別なオプションを追加
    $options = @("help", "exit") + $availableCommands

    try {
        # fzfを使用してコマンドを選択
        $selected = $options | fzf --prompt="コマンドを選択してください > " --height=40% --border --preview-window=hidden
        return $selected
    }
    catch {
        Write-Host "fzfの実行に失敗しました。fzfがインストールされているか確認してください。" -ForegroundColor Red
        return $null
    }
}

# 対話モードの関数
function Start-InteractiveMode {
    Write-Host "fzf対話モードを開始します。" -ForegroundColor Green
    Write-Host "Ctrl+C で終了します。" -ForegroundColor Yellow
    Write-Host ""

    while ($true) {
        $selectedCommand = Select-CommandWithFzf

        if (-not $selectedCommand) {
            Write-Host "コマンドが選択されませんでした。終了します。" -ForegroundColor Yellow
            break
        }

        if ($selectedCommand -eq "exit") {
            Write-Host "対話モードを終了します。" -ForegroundColor Yellow
            break
        }

        if ($selectedCommand -eq "help") {
            Show-Help
            Write-Host ""
            continue
        }

        Invoke-SubCommand -CommandName $selectedCommand -Args @()

        Write-Host ""
        Write-Host "次のコマンドを選択してください..." -ForegroundColor Cyan
        Write-Host ""
    }
}

# メイン処理
if ($SubCommand -eq "help" -or $SubCommand -eq "?") {
    Show-Help
}
elseif ($SubCommand -and $SubCommand -ne "interactive" -and $SubCommand -ne "i") {
    # setup-scriptsディレクトリが存在するかチェック
    if (-not (Test-Path $SubCommandDir)) {
        Write-Host "エラー: setup-scripts ディレクトリが見つかりません。" -ForegroundColor Red
        Write-Host "パス: $SubCommandDir" -ForegroundColor Gray
        exit 1
    }

    Invoke-SubCommand -CommandName $SubCommand -Args $Arguments
}
else {
    # デフォルトで対話モードを開始
    Start-InteractiveMode
}
