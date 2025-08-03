#!/bin/bash

# chezmoi add symlink script
# Usage: ./chezmoi-add-symlink.sh <source_file> <app_name> [target_path]
# Example: ./chezmoi-add-symlink.sh settings.json VSCode ~/.config/Code/User/settings.json

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

show_usage() {
    cat << EOF
Usage: $0 <source_file> <app_name> [target_path]

Arguments:
  source_file   対象ファイルのパス（相対パス・絶対パス可）
  app_name      アプリケーション名（シンボリックリンク先のディレクトリ名）
  target_path   ファイルの本来の場所（省略時は現在のパスを使用）

Examples:
  $0 settings.json VSCode ~/.config/Code/User/settings.json
  $0 ../config/app.conf MyApp
  $0 /home/user/.vimrc Vim ~/.vimrc

このスクリプトは以下の処理を行います：
1. 指定されたファイルをchezmoi source配下のsymlink-srcディレクトリにコピー
2. 元の場所にシンボリックリンクテンプレートを作成
3. chezmoi applyを実行してシンボリックリンクを適用
EOF
}

# Check if chezmoi is installed
check_chezmoi() {
    if ! command -v chezmoi &> /dev/null; then
        log_error "chezmoiがインストールされていません"
        exit 1
    fi
}

# Convert relative path to absolute path
get_absolute_path() {
    local path="$1"
    if [[ "$path" = /* ]]; then
        echo "$path"
    else
        echo "$(cd "$(dirname "$path")" && pwd)/$(basename "$path")"
    fi
}

# Convert absolute path to chezmoi path format
convert_to_chezmoi_path() {
    local abs_path="$1"
    local home_dir="$HOME"

    # Remove home directory prefix and convert to chezmoi format
    local rel_path="${abs_path#$home_dir/}"

    # If path doesn't start with home directory, it's outside home
    if [[ "$abs_path" == "$rel_path" ]]; then
        log_error "ファイルはホームディレクトリ配下にある必要があります: $abs_path"
        exit 1
    fi

    # Convert path components to chezmoi format
    local chezmoi_path=""
    IFS='/' read -ra ADDR <<< "$rel_path"
    for component in "${ADDR[@]}"; do
        if [[ -n "$component" ]]; then
            # Add private_ prefix for hidden directories/files
            if [[ "$component" == .* ]]; then
                component="private_dot_${component#.}"
            fi
            chezmoi_path="${chezmoi_path}/${component}"
        fi
    done

    echo "${chezmoi_path#/}"
}

# Main processing function
process_file() {
    local source_file="$1"
    local app_name="$2"
    local target_path="${3:-}"

    # Get absolute path of source file
    local abs_source_path
    abs_source_path=$(get_absolute_path "$source_file")

    # Check if source file exists
    if [[ ! -f "$abs_source_path" ]]; then
        log_error "ソースファイルが存在しません: $abs_source_path"
        exit 1
    fi

    # Determine target path
    if [[ -z "$target_path" ]]; then
        target_path="$abs_source_path"
        log_info "ターゲットパスが指定されていないため、ソースパスを使用: $target_path"
    else
        target_path=$(get_absolute_path "$target_path")
    fi

    # Get chezmoi source directory
    local chezmoi_source_dir
    chezmoi_source_dir=$(chezmoi source-path)

    # Create symlink-src directory structure
    local symlink_src_dir="$chezmoi_source_dir/symlink-src/$app_name"
    log_info "シンボリックリンクソースディレクトリを作成: $symlink_src_dir"
    mkdir -p "$symlink_src_dir"

    # Copy source file to symlink-src directory
    local filename
    filename=$(basename "$abs_source_path")
    local symlink_target="$symlink_src_dir/$filename"

    log_info "ファイルをコピー: $abs_source_path -> $symlink_target"
    cp "$abs_source_path" "$symlink_target"

    # Convert target path to chezmoi format
    local chezmoi_target_path
    chezmoi_target_path=$(convert_to_chezmoi_path "$target_path")

    # Create directory structure in chezmoi source
    local chezmoi_dir="$chezmoi_source_dir/$(dirname "$chezmoi_target_path")"
    log_info "chezmoiディレクトリ構造を作成: $chezmoi_dir"
    mkdir -p "$chezmoi_dir"

    # Create symlink template
    local template_name="symlink_$(basename "$chezmoi_target_path").tmpl"
    local template_path="$chezmoi_dir/$template_name"

    log_info "シンボリックリンクテンプレートを作成: $template_path"
    echo -n "{{ .chezmoi.sourceDir }}/symlink-src/$app_name/$filename" > "$template_path"

    log_success "設定完了！"
    log_info "以下のファイルが作成されました："
    log_info "  - シンボリックリンクソース: $symlink_target"
    log_info "  - テンプレート: $template_path"

    # Ask if user wants to apply changes
    read -p "chezmoi applyを実行しますか？ (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        log_info "chezmoi applyを実行中..."
        if chezmoi apply -v; then
            log_success "chezmoi applyが完了しました"
            log_info "これで $target_path は $symlink_target へのシンボリックリンクになりました"
        else
            log_error "chezmoi applyでエラーが発生しました"
            exit 1
        fi
    else
        log_info "後で手動で 'chezmoi apply' を実行してください"
    fi
}

# Main script
main() {
    # Check arguments
    if [[ $# -lt 2 || $# -gt 3 ]]; then
        log_error "引数の数が正しくありません"
        show_usage
        exit 1
    fi

    # Show help
    if [[ "$1" == "-h" || "$1" == "--help" ]]; then
        show_usage
        exit 0
    fi

    # Check if chezmoi is installed
    check_chezmoi

    # Process the file
    process_file "$1" "$2" "${3:-}"
}

# Run main function with all arguments
main "$@"
