# クリップボードの操作に必要なアセンブリを読み込み
Add-Type -AssemblyName System.Windows.Forms

# Pandocの存在確認
if (-not (Get-Command pandoc -ErrorAction SilentlyContinue)) {
    Write-Host "Error: Pandocがインストールされていません。" -ForegroundColor Red
    exit 1
}

# クリップボードのデータを取得
$clipboard = [System.Windows.Forms.Clipboard]::GetDataObject()

# HTMLフォーマットのデータを取得（なければプレーンテキスト）
$html = if ($clipboard.GetDataPresent("HTML Format")) {
    $rawHtml = $clipboard.GetData("HTML Format")
    # メタデータヘッダーを除去
    if ($rawHtml -match "(?s).*?(<html.*?>.*</html>).*") {
        $matches[1]
    } elseif ($rawHtml -match "(?s).*?<\!--StartFragment-->(.*?)<\!--EndFragment-->.*") {
        $matches[1]
    } else {
        $rawHtml
    }
} elseif ($clipboard.GetDataPresent("UnicodeText")) {
    $clipboard.GetData("UnicodeText")
} else {
    Write-Host "Error: クリップボードにテキストデータが見つかりません。" -ForegroundColor Red
    exit 1
}

# 一時ファイルのパスを生成
$tempHtmlPath = [System.IO.Path]::GetTempFileName() + ".html"
$tempMdPath = [System.IO.Path]::GetTempFileName() + ".md"

try {
    # HTMLを一時ファイルに保存
    $html | Out-File -Encoding utf8 $tempHtmlPath

    # pandocを使用してMarkdownに変換
    $result = pandoc -f html -t markdown_strict --wrap=none $tempHtmlPath -o $tempMdPath 2>&1

    if ($LASTEXITCODE -eq 0) {
        # 変換されたMarkdownをクリップボードにコピー
        $markdown = Get-Content -Raw $tempMdPath
        [System.Windows.Forms.Clipboard]::SetText($markdown)
        Write-Host "HTMLからMarkdownへの変換が完了しました。" -ForegroundColor Green
    } else {
        Write-Host "Error: Pandocでの変換中にエラーが発生しました。" -ForegroundColor Red
        Write-Host $result
    }
} catch {
    Write-Host "Error: 処理中にエラーが発生しました: $_" -ForegroundColor Red
} finally {
    # 一時ファイルの削除
    if (Test-Path $tempHtmlPath) { Remove-Item $tempHtmlPath }
    if (Test-Path $tempMdPath) { Remove-Item $tempMdPath }
}
