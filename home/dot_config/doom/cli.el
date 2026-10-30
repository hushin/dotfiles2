;;; cli.el -*- lexical-binding: t; -*-

;; Windows の Emacs は子プロセスに LANG=JPN を渡すため、doom/reload から起動した bin/doom の既定が
;; Shift-JIS になる。終了時に ✓ を含むログを書けず Select coding system で入力待ちになり、reload が失敗する
(prefer-coding-system 'utf-8)
