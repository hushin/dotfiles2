-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- fishをデフォルトシェルに設定
config.default_prog = { '/opt/homebrew/bin/fish', '-l' }

-- 基本設定
config.color_scheme = 'Dracula'
config.font = wezterm.font('UDEV Gothic NF', { weight = 'Medium' })
config.font_size = 14.0
config.initial_cols = 180
config.initial_rows = 40

-- ウィンドウ設定
config.window_background_opacity = 0.95
config.macos_window_background_blur = 20

-- タイトルバーを非表示
config.window_decorations = "RESIZE"
-- タブが一つの時は非表示
config.hide_tab_bar_if_only_one_tab = true

config.keys = {
  {
    key = 'd',
    mods = 'CMD',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'd',
    mods = 'CMD|SHIFT',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  -- ⌘ + でフォントサイズを大きくする
  {
    key = "+",
    mods = "CMD|SHIFT",
    action = wezterm.action.IncreaseFontSize,
  },
  -- ⌘ w でペインを閉じる（デフォルトではタブが閉じる）
  {
    key = "w",
    mods = "CMD",
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
  -- ⌘ [ で左のペインに移動
  {
    key = "[",
    mods = "CMD",
    action = wezterm.action.ActivatePaneDirection 'Prev',
  },
  -- ⌘ ] で右のペインに移動
  {
    key = "]",
    mods = "CMD",
    action = wezterm.action.ActivatePaneDirection 'Next',
  },
  -- ⌘ k でターミナルをクリア
  {
    key = "k",
    mods = "CMD",
    action = wezterm.action.ClearScrollback 'ScrollbackAndViewport',
  },
}

-- マウス操作の挙動設定
config.mouse_bindings = {
  -- 右クリックでクリップボードから貼り付け
  {
    event = { Down = { streak = 1, button = 'Right' } },
    mods = 'NONE',
    action = wezterm.action.PasteFrom 'Clipboard',
  },
}

return config
