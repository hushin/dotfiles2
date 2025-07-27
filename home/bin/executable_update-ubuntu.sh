#!/bin/bash

# Ubuntu package update script
# Updates system packages, snaps, and flatpaks

set -e

echo "🔄 Starting Ubuntu package updates..."

# Update package list
echo "📦 Updating package list..."
sudo apt update

# Upgrade packages
echo "⬆️ Upgrading packages..."
sudo apt upgrade -y

# Autoremove unused packages
echo "🧹 Removing unused packages..."
sudo apt autoremove -y

# Autoclean package cache
echo "🗑️ Cleaning package cache..."
sudo apt autoclean

# Update the executable update script
echo "🔄 Updating mise self-update..."
mise self-update
# Update mise
echo "🔄 Updating mise..."
mise upgrade

# Update snap packages if snap is available
if command -v snap &> /dev/null; then
    echo "📱 Updating snap packages..."
    sudo snap refresh
fi

# Update flatpak packages if flatpak is available
if command -v flatpak &> /dev/null; then
    echo "📦 Updating flatpak packages..."
    flatpak update -y
fi

echo "✅ All updates completed!"
