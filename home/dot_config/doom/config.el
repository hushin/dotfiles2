;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ここに個人設定を記述してください！このファイルを変更した後に 'doom
;; sync' を実行する必要はありません！


;; 一部の機能では、GPG設定、メールクライアント、ファイルテンプレート、
;; スニペットなどで、ユーザーを識別するためにこれを使用します。オプションです。
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doomでフォントを制御するための5つの（オプション）変数が用意されています：
;;
;; - `doom-font' -- 使用するメインフォント
;; - `doom-variable-pitch-font' -- 非等幅フォント（該当する場合）
;; - `doom-big-font' -- `doom-big-font-mode' で使用；プレゼンテーションや
;;   ストリーミングに使用
;; - `doom-symbol-font' -- シンボル用
;; - `doom-serif-font' -- `fixed-pitch-serif' フェイス用
;;
;; これらが受け入れる値のドキュメントと例については、'C-h v doom-font' を参照してください。
;; 例：
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; フォントが見つからない場合は、'M-x describe-font' で検索し、
;; `M-x eval-region' でelispコードを実行し、'M-x doom/reload-font' で
;; フォント設定を更新してください。それでもEmacsがフォントを見つけられない場合は、
;; 正しくインストールされていない可能性があります。フォントの問題はDoomの問題ではありません！

(setq doom-font (font-spec :family "UDEV Gothic NF" :size (if (featurep :system 'windows) 26 16)))

;; テーマを読み込む方法は2つあります。どちらもテーマがインストールされて
;; 利用可能であることを前提としています。`doom-theme' を設定するか、
;; `load-theme' 関数で手動でテーマを読み込むことができます。これがデフォルトです：
(setq doom-theme 'doom-nord)

;; これは行番号のスタイルを決定します。`nil' に設定すると、行番号は
;; 無効になります。相対行番号の場合は、これを `relative' に設定してください。
(setq display-line-numbers-type nil)

;; `org' を使用していて、orgファイルを以下のデフォルトの場所に置きたくない場合は、
;; `org-directory' を変更してください。orgが読み込まれる前に設定する必要があります！
(setq org-directory "~/Documents/memo/org/")
(setq org-roam-directory org-directory)
(setq org-roam-file-exclude-regexp "/archives/")

;; パッケージを再設定する際は、設定を `after!' ブロックで囲むようにしてください。
;; そうしないと、Doomのデフォルト設定が上書きされる可能性があります。例：
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; このルールの例外：
;;
;;   - ファイル/ディレクトリ変数の設定（`org-directory' など）
;;   - パッケージが読み込まれる前に設定することが明示的に指示されている変数の設定
;;     （ドキュメントを参照するには 'C-h v VARIABLE' を使用）
;;   - doom変数の設定（'doom-' または '+' で始まる変数）
;;
;; Doomの設定に役立つ追加の関数/マクロをご紹介します。
;;
;; - `load!' このファイルに相対的な外部の*.elファイルを読み込む
;; - `use-package!' パッケージを設定する
;; - `after!' パッケージが読み込まれた後にコードを実行する
;; - `add-load-path!' このファイルに相対的なディレクトリを `load-path' に追加する
;;   Emacsは `require' や `use-package' でパッケージを読み込む際に `load-path' を検索します
;; - `map!' 新しいキーをバインドする
;;
;; これらの関数/マクロの情報を取得するには、ハイライトされたシンボルにカーソルを
;; 移動して 'K' を押してください（non-evilユーザーは 'C-c c k' を押す）。
;; 使用方法のデモを含むドキュメントが開きます。
;; または、`C-h o' を使用してシンボル（関数、変数、フェイスなど）を検索することもできます。
;;
;; 'gd'（または 'C-c c d'）を試して定義にジャンプし、実装方法を確認することもできます。


;; Ctrl-h
;(map! "C-h" 'delete-backward-char)

;; delete character without yanking
(map! :n "x" 'delete-char)

;; leader key
(add-hook! 'org-mode-hook #'+org-init-keybinds-h)
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; auto save
(use-package! super-save
  :config
  (setq super-save-auto-save-when-idle t
    super-save-idle-duration 1)
  (super-save-mode +1)
  )

;; Disable exit confirmation.
(setq confirm-kill-emacs nil)

;; org-mode の日付を英語にする
(setq system-time-locale "C")
;; UTF-8をデフォルトのエンコーディングとして設定
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (featurep :system 'windows)
  ;; Windows 用のクリップボード設定
  (set-selection-coding-system 'utf-16le-dos)
  ;; Windows search-project で 日本語で検索できるようにする
  (defun advice:with-japanese-coding-system (orig-fun &rest args)
    (let ((coding-system-for-write 'cp932))
      (apply orig-fun args)))
  (advice-add '+default/search-project :around 'advice:with-japanese-coding-system))

(setq calendar-week-start-day 1)
;; Displaying week numbers in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0
                    :foreground "gray50")
(setq calendar-intermonth-text
  '(propertize
     (format "w%2d"
       (car
         (calendar-iso-from-absolute
           (calendar-absolute-from-gregorian
             (list month (- day (1- calendar-week-start-day)) year)))))
     'font-lock-face 'calendar-iso-week-face))

(setq markdown-fontify-code-blocks-natively t)

;; ファイル操作関数
(load! "lib/file-operations")

;; org-agenda 設定
(load! "lib/org-agenda-config")

(after! org
  (map!
    "C-c a" #'org-agenda
    "C-c c" #'org-capture
    "C-c j" #'org-journal-new-entry
    )
  (setq org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
             (sequence "WAITING(w/!)" "|" "CANCELED(c/!)"))))
  (setq org-log-done 'time)
  ; 見出し入れるときは空行を入れない
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (defun my/property-values-function (property)
    "Return allowed values for PROPERTY."
    (cond
      ((string= property "Type")
        '("Book" "Web" "Anime" "Game" "Podcast" "Video" "Movie"))
      ((string= property "Rating")
        '("*" "**" "***" "****" "*****"))
      ((string= property "Canceled")
        '("true" ""))
      ))

  (setq org-property-allowed-value-functions
    '(my/property-values-function))

  (setq org-capture-templates
    '(
       ("t" "Task" entry (file+headline "gtd.org" "Inbox")
         "* TODO %? \nCREATED: %U\n %i")
       ("n" "Task NEXT" entry (file+headline "gtd.org" "Inbox")
         "* NEXT %? \nCREATED: %U\n %i ")
       ("T" "Task from protocol" entry (file+headline "gtd.org" "Inbox")
         "* TODO %? [[%:link][%:description]] \nCREATED: %U\n%i\n\n")
       ("L" "ReadItLater" entry (file+headline "gtd.org" "ReadItLater")
         "* TODO %? [[%:link][%:description]] \nCREATED: %U\n%i\n")
       ))


  ;; journal
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%Y-%m-%d %A")
  ;; (setq org-journal-time-format "%R ")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-find-file 'find-file)
  (setq org-extend-today-until '3)
  (add-hook 'org-journal-after-entry-create-hook 'evil-insert-state)
  (setq org-startup-with-inline-images t)

  (setq org-M-RET-may-split-line '((item . t)))
  ;; export周りの設定
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-creator nil)
  (setq org-use-sub-superscripts nil)
  (setq org-export-with-sub-superscripts nil)

  (load! "lib/task-collection")
  (load! "lib/org-bullet-functions")
  )

(map! :after evil-org
  :map evil-org-mode-map
  :ni "C-<return>" #'org-insert-heading-respect-content
  :ni "C-S-<return>" #'org-insert-todo-heading-respect-content
  :ni "M-<left>" #'org-metaleft
  :ni "M-<right>" #'org-metaright
  )

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  )

(after! org-roam
  (map!
    "C-c n l" #'org-roam-buffer-toggle
    "C-c n f" #'org-roam-node-find
    "C-c l" #'org-roam-dailies-goto-today
    "C-c d" #'org-roam-dailies-map
    :n "[ D" #'org-roam-dailies-goto-previous-note
    :n "] D" #'org-roam-dailies-goto-next-note
    :map org-mode-map
    "C-c n i" #'org-roam-node-insert
    "C-M-i" #'completion-at-point
    )
  (setq org-roam-completion-everywhere nil)

  (setq org-roam-node-display-template
    (format "%s ${doom-hierarchy:*} %s"
      (propertize "${doom-type:15}" 'face 'font-lock-keyword-face)
      (propertize "${doom-tags:10}" 'face '(:inherit org-tag :box nil))))

  (load! "lib/org-roam-templates")
  (load! "lib/weekly-review")
  (load! "lib/org-timestamp")
  (load! "lib/org-file-browser")
  )

(after! org-roam-protocol
  (defun org-roam-protocol-open-ref (info)
    "Process an org-protocol://roam-ref?ref= style url with INFO."
    (let ((org-capture-link-is-already-stored t))
      (org-roam-capture-
        :keys (plist-get info :template)
        :node (org-roam-node-create :title (plist-get info :title))
        :info (list :ref (plist-get info :ref)
                :body (plist-get info :body)
                ;; 独自の変数を送れるように追加
                :type (plist-get info :type)
                :creator (plist-get info :creator)
                :releaseDate (plist-get info :releaseDate)
                )
        :templates org-roam-capture-ref-templates))
    nil)
  )

(after! org-download
  (when (winp)
    (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")
    )
  )

(use-package! org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;; evil keymap https://github.com/alphapapa/org-super-agenda/issues/50
  (setq org-super-agenda-header-map (make-sparse-keymap))
  )

(use-package! org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode))
