#!/bin/bash

set -e

echo "🚀 Starting comprehensive macOS update process..."
echo "================================================"

# Function to print section headers
print_section() {
    echo ""
    echo "📦 $1"
    echo "----------------------------------------"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Update macOS system
print_section "Updating macOS system"
if command_exists softwareupdate; then
    echo "Checking for macOS updates..."
    softwareupdate -ia
else
    echo "⚠️  softwareupdate command not found"
fi

# Update Homebrew
print_section "Updating Homebrew"
if command_exists brew; then
    echo "Updating Homebrew..."
    brew update
    echo "Upgrading Homebrew packages..."
    brew upgrade
    echo "Cleaning up Homebrew..."
    brew cleanup
    echo "Running Homebrew doctor..."
    brew doctor || true
else
    echo "⚠️  Homebrew not installed"
fi

# Update mise
print_section "Updating mise"
if command_exists mise; then
    echo "Upgrading mise tools..."
    mise upgrade
else
    echo "⚠️  mise not installed"
fi

# Update App Store apps
print_section "Updating App Store apps"
if command_exists mas; then
    echo "Updating App Store apps..."
    mas upgrade
else
    echo "⚠️  mas (Mac App Store CLI) not installed"
fi

# Update chezmoi
print_section "Updating chezmoi"
if command_exists chezmoi; then
    echo "Updating chezmoi..."
    chezmoi upgrade
else
    echo "⚠️  chezmoi not available"
fi

echo ""
echo "✅ All updates completed!"
echo "================================================"
echo "💡 Consider running 'chezmoi apply' if dotfiles were updated"
