#!/bin/bash

# Dependency: This script requires the `pandoc` cli to be installed: https://pandoc.org/
# Install via homebrew: `brew install pandoc`

# MIT License
# based on https://github.com/raycast/script-commands/blob/master/commands/conversions/rich-text-clipboard-to-markdown.sh

# @raycast.title Markdown to org-mode
# @raycast.author hushin
# @raycast.authorURL https://github.com/hushin
# @raycast.description Convert markdown clipboard data to org-mode format using Pandoc
#
# @raycast.icon 📝
#
# @raycast.mode silent
# @raycast.packageName Conversions
# @raycast.schemaVersion 1

if ! command -v pandoc &> /dev/null; then
    echo "pandoc is required (https://pandoc.org/).";
    exit 1;
fi

export LC_CTYPE=UTF-8
# クリップボードからテキストを取得
pbpaste | \
pandoc --from=markdown --to=org | \
# 必要に応じて整形（PROPERTIESブロックの削除）
perl -0777 -pe 's/^\:PROPERTIES\:\n(.*?)\n\:END\:\n//gms' | \
# 見出しレベルを一段下げて先頭に「* md2org」を追加（見出しは行頭の*とスペースのパターンのみ対象）
perl -pe 's/^(\*+)(\s)/\1*$2/g' | \
{ echo "* md2org"; cat -; } | \
pbcopy

echo "Markdown converted to org-mode format and copied to clipboard!"
