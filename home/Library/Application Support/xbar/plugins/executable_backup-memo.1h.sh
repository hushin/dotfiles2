#!/bin/bash

eval $(/opt/homebrew/bin/brew shellenv)

# rcloneを使用して、ローカルのmemoディレクトリをGoogle Driveにバックアップする
output=$(rclone sync ~/Documents/memo GoogleDrive:memo --exclude .git/** 2>&1)

if [ $? -eq 0 ]; then
    echo "D | color=green"
else
    # 失敗した場合、エラーメッセージを含めて表示
    echo "F | color=red"
    echo "---"
    echo $output
fi
