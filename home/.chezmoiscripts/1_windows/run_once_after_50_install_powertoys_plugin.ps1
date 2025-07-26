# NOTE  .chezmoiexternal.toml.tmpl で 設定したかったが、うまく動かなかったのでscriptを作成

$pluginsDir = "$HOME\AppData\Local\Microsoft\PowerToys\PowerToys Run\Plugins"

# プラグイン情報
$plugins = @(
    @{
        Name = "ClipboardManager"
        Url = "https://github.com/CoreyHayward/PowerToys-Run-ClipboardManager/releases/download/v0.11.0/ClipboardManager-0.11.0-x64.zip"
    },
    @{
        Name = "GEmojiSharp"
        Url = "https://github.com/hlaueriksson/GEmojiSharp/releases/download/v4.0.1/GEmojiSharp.PowerToysRun-4.0.1-x64.zip"
    }
)

# プラグインディレクトリが存在しない場合は作成
if (-not (Test-Path $pluginsDir)) {
    New-Item -ItemType Directory -Path $pluginsDir -Force | Out-Null
    Write-Host "Created plugins directory: $pluginsDir"
}

foreach ($plugin in $plugins) {
    $pluginPath = Join-Path $pluginsDir $plugin.Name
    
    # プラグインディレクトリが既に存在する場合はスキップ
    if (Test-Path $pluginPath) {
        Write-Host "Plugin $($plugin.Name) already exists, skipping..."
        continue
    }
    
    Write-Host "Installing plugin: $($plugin.Name)"
    
    # 一時ファイルパス
    $tempZip = Join-Path $env:TEMP "$($plugin.Name).zip"
    
    try {
        # ZIPファイルをダウンロード
        Invoke-WebRequest -Uri $plugin.Url -OutFile $tempZip -UseBasicParsing
        
        # 一時解凍ディレクトリ
        $tempExtractDir = Join-Path $env:TEMP "$($plugin.Name)_extract"
        
        # ZIPファイルを一時ディレクトリに解凍
        Expand-Archive -Path $tempZip -DestinationPath $tempExtractDir -Force
        
        # 2階層目のファイルをプラグインディレクトリにコピー
        $extractedItems = Get-ChildItem $tempExtractDir -Recurse -File
        if ($extractedItems.Count -gt 0) {
            # プラグインディレクトリを作成
            New-Item -ItemType Directory -Path $pluginPath -Force | Out-Null
            
            # 最初に見つかったサブディレクトリの内容をコピー
            $subDir = Get-ChildItem $tempExtractDir -Directory | Select-Object -First 1
            if ($subDir) {
                Copy-Item -Path "$($subDir.FullName)\*" -Destination $pluginPath -Recurse -Force
            } else {
                # サブディレクトリがない場合は直接コピー
                Copy-Item -Path "$tempExtractDir\*" -Destination $pluginPath -Recurse -Force
            }
        }
        
        # 一時解凍ディレクトリを削除
        if (Test-Path $tempExtractDir) {
            Remove-Item $tempExtractDir -Recurse -Force
        }
        
        Write-Host "Successfully installed plugin: $($plugin.Name)"
    }
    catch {
        Write-Error "Failed to install plugin $($plugin.Name): $_"
    }
    finally {
        # 一時ファイルを削除
        if (Test-Path $tempZip) {
            Remove-Item $tempZip -Force
        }
    }
}

Write-Host "PowerToys plugin installation completed."

