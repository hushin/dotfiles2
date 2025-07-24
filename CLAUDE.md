# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a dotfiles repository managed by chezmoi for cross-platform configuration management (Linux, macOS, Windows).

## Key Commands

### Development and Setup

- `chezmoi apply` - Apply configuration changes to the destination
- `chezmoi diff` - Show diff between current state and target
- `chezmoi status` - Show status of managed files
- `chezmoi edit <file>` - Edit a file in the source state
- `chezmoi re-add <file>` - Re-add modified files
- `chezmoi cd` - Change to the source directory

### Testing and Verification

- `chezmoi verify` - Verify destination state matches target state
- `chezmoi doctor` - Check system for potential problems

### Git Operations (within chezmoi source)

- `chezmoi git status` - Git status in source directory
- `chezmoi git add .` - Git add in source directory
- `chezmoi git commit -m "message"` - Git commit in source directory

## Architecture

### Directory Structure

- `home/` - Contains dotfiles that will be placed in user's home directory
  - `dot_*` files become `.` prefixed files in home directory
  - `.chezmoiscripts/` - Platform-specific setup scripts
- `docs/` - Documentation files
- `etc/` - Miscellaneous configuration files (e.g., Surfingkeys)

### Platform-Specific Scripts

Scripts are organized by platform in `home/.chezmoiscripts/`:

- `1_darwin/` - macOS setup scripts
- `1_linux/` - Linux setup scripts
- `1_windows/` - Windows setup scripts (PowerShell)
- `2_fish/` - Fish shell setup scripts

### Key Configuration Files

- `home/dot_config/mise/config.toml` - Development tools and language versions
- `home/dot_config/git/config` - Git configuration with extensive aliases
- `home/dot_config/fish/` - Fish shell configuration and functions
- `home/dot_config/tmux/tmux.conf` - Tmux configuration

### Template System

Chezmoi uses Go templates for dynamic configuration:

- `.tmpl` files are processed as templates
- Template data can include OS, architecture, and user-specific variables

## Development Tools Managed

- Languages: Node.js (LTS), Python, Go, Rust, Deno, Bun
- CLI tools: ripgrep, bat, fzf, gh, lazygit, delta, fd
- Development: jq, starship, 1password-cli

## Windows-Specific Setup

- Winget package installation via PowerShell scripts
- AutoHotkey configuration for JIS keyboard remapping
- Windows debloating and customization scripts
