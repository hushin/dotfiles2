;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; OS判定
(defun macp ()
  (eq system-type 'darwin))
(defun linuxp ()
  (eq system-type 'gnu/linux))
(defun bsdp ()
  (eq system-type 'gnu/kfreebsd))
(defun winp ()
  (eq system-type 'windows-nt))
(defun wslp ()
  (and (eq system-type 'gnu/linux)
    (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

(setq doom-font (font-spec :family "UDEV Gothic NF" :size (if (winp) 24 18))
  doom-variable-pitch-font (font-spec :family "UDEV Gothic NF"))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/memo/org/")
(setq org-roam-directory org-directory)
(setq org-roam-file-exclude-regexp "/archives/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



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
;; Windows 用のクリップボード設定
(when (winp)
  (set-selection-coding-system 'utf-16le-dos))
;; Windows search-project で 日本語で検索できるようにする
(when (winp)
  (defun advice:with-japanese-coding-system (orig-fun &rest args)
    (let ((coding-system-for-write 'cp932))
      (apply orig-fun args)))
  (advice-add '+default/search-project :around 'advice:with-japanese-coding-system))

;; Displaying week numbers in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
  :height 0.8)
(setq calendar-intermonth-text
  '(propertize
     (format "w%2d"
       (car
         (calendar-iso-from-absolute
           (calendar-absolute-from-gregorian
             (list month (- day (1- calendar-week-start-day)) year)))))
     'font-lock-face 'calendar-iso-week-face))

;; ファイル操作関数
(load! "file-operations")

;; org-agenda 設定
(load! "org-agenda-config")

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

  ;; C-S-Enter で見出し作ったときにタイムスタンプをつける
  (defun my/org-insert-created-timestamp ()
    "Insert timestamp as plain text after heading"
    (interactive)
    (let ((heading-pos (point)))  ; 現在位置（見出し）を保存
      (org-back-to-heading t)
      (end-of-line)
      (insert "\nCREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (goto-char heading-pos)     ; 元の位置（見出し末尾）に戻る
      (end-of-line)))            ; 見出しの末尾に移動
  (defun my/org-mode-timestamp-on ()
     "Enable automatic timestamp insertion for new todo headings"
     (advice-add 'org-insert-todo-heading-respect-content
       :after #'my/org-insert-created-timestamp))

  (defun my/org-mode-timestamp-off ()
     "Disable automatic timestamp insertion for new todo headings"
     (advice-remove 'org-insert-todo-heading-respect-content
       #'my/org-insert-created-timestamp))

  (defun my/org-mode-timestamp-toggle ()
     "Toggle automatic timestamp insertion for new todo headings"
     (interactive)
     (if (advice-member-p #'my/org-insert-created-timestamp 'org-insert-todo-heading-respect-content)
       (progn
         (my/org-mode-timestamp-off)
         (message "Timestamp insertion disabled"))
       (progn
         (my/org-mode-timestamp-on)
         (message "Timestamp insertion enabled"))))
  (global-set-key (kbd "C-c C-x t") #'my/org-mode-timestamp-toggle)

  ;; journal
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%Y-%m-%d %A")
  ;; (setq org-journal-time-format "%R ")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-find-file 'find-file)
  (setq org-extend-today-until '3)
  (add-hook 'org-journal-after-entry-create-hook 'evil-insert-state)
  (setq org-startup-with-inline-images t)

  (setq org-M-RET-may-split-line t)
  ;; export周りの設定
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-creator nil)
  (setq org-use-sub-superscripts nil)
  (setq org-export-with-sub-superscripts nil)

  (load! "task-collection")
  (defun my/org-bullet-to-heading-region (begin end)
    "Convert bullet points to headings in the selected region.
The heading level will be parent heading level + 1 + indent level."
    (interactive "r")
    (save-excursion
      (let* ((current-heading (save-excursion
                                (goto-char begin)
                                (org-back-to-heading t)
                                (org-element-at-point)))
              (parent-level (or (org-element-property :level current-heading) 0))
              (base-level (1+ parent-level)))

        ;; Move to the beginning of the region
        (goto-char begin)
        (beginning-of-line)

        ;; First pass: collect all indentation levels
        (let ((indent-levels (make-hash-table :test 'equal))
               (min-indent nil))
          ;; Collect unique indentation levels
          (save-excursion
            (while (and (<= (point) end)
                     (not (eobp)))
              (when (looking-at "^\\([ ]*\\)[-+*] ")
                (let ((indent (length (match-string 1))))
                  (puthash indent t indent-levels)
                  (when (or (null min-indent) (< indent min-indent))
                    (setq min-indent indent))))
              (forward-line 1)))

          ;; Convert indent levels to heading levels
          (let ((level-map (make-hash-table :test 'equal)))
            (let ((current-level base-level))
              (dolist (indent (sort (hash-table-keys indent-levels) '<))
                (puthash indent current-level level-map)
                (setq current-level (1+ current-level))))

            ;; Second pass: convert bullets to headings
            (goto-char begin)
            (while (and (<= (point) end)
                     (not (eobp)))
              (when (looking-at "^\\([ ]*\\)\\([-+*]\\) \\(.+\\)$")
                (let* ((indent (length (match-string 1)))
                        (content (match-string 3))
                        (heading-level (gethash indent level-map base-level))
                        (stars (make-string heading-level ?*)))
                  ;; Replace the bullet point with heading
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert (format "%s %s" stars content))))
              (forward-line 1)))))))
  (defun my/org-bullet-to-heading-toplevel-region (begin end)
    "Convert top-level bullet points to headings in the selected region.
The heading level will be parent heading level + 1.
Also reduces indentation of nested bullet points by 2 spaces."
    (interactive "r")
    (save-excursion
      (let* ((current-heading (save-excursion
                                (goto-char begin)
                                (org-back-to-heading t)
                                (org-element-at-point)))
              (parent-level (or (org-element-property :level current-heading) 0))
              (new-level (1+ parent-level)))

        ;; Move to the beginning of the region
        (goto-char begin)
        (beginning-of-line)

        ;; Process each line in the region
        (while (and (< (point) (- end 2))
                 (not (eobp)))
          (cond
            ;; Convert top-level bullet points to headings
            ((looking-at "^\\([-+*]\\) \\(.+\\)$")
              (let* ((content (match-string 2))
                      (stars (make-string new-level ?*)))
                (delete-region (line-beginning-position) (line-end-position))
                (insert (format "%s %s" stars content))))

            ;; Reduce indentation of nested bullet points
            ((looking-at "^\\([ ]+\\)\\([-+*]\\) \\(.+\\)$")
              (let* ((indent (match-string 1))
                      (bullet (match-string 2))
                      (content (match-string 3))
                      (new-indent (max 0 (- (length indent) 2))))
                (delete-region (line-beginning-position) (line-end-position))
                (insert (format "%s%s %s"
                          (make-string new-indent ?\s)
                          bullet
                          content)))))
          (forward-line 1)))))
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
  (setq org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M> %?"
        :target (file+head "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>

* 今日の目標

* 思い+タスク
# - 理想の制御：理想の今日のイメージをクリアにする
#   - 1. フリーライティングする
#     - 理想の一日のイメージを作る
#     - カレンダーをチェックして外せない用事をチェック
#     - 思いついたことを文章として書く
#   - 2. 仮アウトラインを作る
#   - 3. 仮サマリーを作る


** TODO 日記を書く
** TODO エントリー消化

* できれば
"))
       )
    )
  (setq org-roam-capture-templates
    '(
       ("d" "default (Zettelkasten Permanent)" plain
         "%?"
         :target (file+head "zk/%<%Y%m%d%H%M%S>-${slug}.org"
                   "#+title: ${title}
#+filetags: :draft:")
         :unnarrowed t)
       ("p" "project" plain
         "* 目的・目標
- %?

* Tasks
** TODO Add initial tasks

* Notes
"
         :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
       ("a" "area" plain
         "* 目標
# このエリアで達成したい長期的な目標や維持すべき基準
%?

* Projects

* Someday Projects
:PROPERTIES:
:CATEGORY: Someday PJ
:END:

* 定期タスク

* Resource

"
         :target (file+head "areas/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
       ("r" "rez (Resonance Calendar)" plain "* ${title}
:PROPERTIES:
:Type: %?
:Start: <%<%Y-%m-%d %a>>
:Fin:
:Canceled:
:Rating:
:Creator:
:URL:
:ReleaseDate:
:END:

** Why
# なぜ読もう・見ようと思ったのか

** Tasks
*** TODO 読む・見る

** Key Ideas

** Review

** Quotes

** Notes

"
         :target (file+head "resources/rez/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
       ("e" "exp (Experiments)" plain "* ${title}
:PROPERTIES:
:Type: Exp
:Start:
:Fin:
:Assess: <yyyy-mm-dd aaa>
:Qs:
:Status:
:Outcome:
:END:

** Tasks

** 仮説

** 実験手順

** 観察とメモ

** 結果と考察

** 注意点

** 次の実験アイデア

"
         :target (file+head "resources/rez/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
       )
    )

  (defun my/create-weekly-review ()
    "Create a new org-roam file for the current week's review."
    (interactive)
    (let* ((week-number (format-time-string "%V"))
            (year (format-time-string "%Y"))
            (slug (concat year "w" week-number))
            (file-path (format "weekly/%s.org" slug))
            (title (format "Weekly Review %sw%s" year week-number)))
      (my/org-roam-capture-weekly title file-path)))

  (defun my/create-previous-weekly-review ()
    "Create a new org-roam file for the previous week's review."
    (interactive)
    (let* ((week-number (string-to-number (format-time-string "%V")))
            (year (string-to-number (format-time-string "%Y")))
            (previous-week (if (= week-number 1)
                             52
                             (1- week-number)))
            (adjusted-year (if (= week-number 1)
                             (1- year)
                             year))
            (slug (concat (number-to-string adjusted-year) "w" (format "%02d" previous-week)))
            (file-path (format "weekly/%s.org" slug))
            (title (format "Weekly Review %sw%s" adjusted-year (format "%02d" previous-week))))
      (my/org-roam-capture-weekly title file-path)))

  (defun my/org-roam-capture-weekly (title file-path)
    "Helper function to capture a weekly review in org-roam."
    (org-roam-capture-
      :node (org-roam-node-create :title title)
      :props '(:finalize find-file)
      :templates
      `(("w" "weekly review" plain
          "* Review
レビュー日 <%<%Y-%m-%d %a>>
** 今週振り返り
# 日記を読み返して振り返る
*** Good
*** Problem
*** 来週達成したいこと
** 明確にする
*** 把握する
Inboxに収集する
- [ ] Keep memo
- [ ] 紙の書類
- [ ] コミュニケーションツール
  - [ ] メール Inbox
  - [ ] メール ads
  - [ ] LINE
  - [ ] チャットツール
- [ ] カレンダーの予定を確認し、適切なアクションを登録する
*** 頭を空っぽにする
- [ ] 5分で新しいプロジェクト、連絡待ちの事柄、いつかやろうと思っていることなどを書き出し、頭を空にする
** 見極める
*** Inboxを空にする
- [ ] Inboxに入っている明らかになっていないものを仕分け、Inboxを空にする
  - すぐにできるものは実行
*** アクションリストを見直す
=C-c a a= でタスクを見る
- 完了したアクションはDONEにする
- 必要があればリマインダーをセットする
- WAITINGリストの更新
- 参考資料などを適切な場所に保存
*** 次に取るべき行動を考える
- [ ] アクションを推進するためのタスクを定義して追加する
  - プロジェクトリストや行動できていない項目について、次に取るべき行動は何かを考える
  - （たまに） M-x areas, projects で棚卸
- [ ] Resonance Calendar =C-c a r= で読みたい本や映画の予定を立てる
** 整理する
- [ ] プロジェクトリストの更新
  - 長期的なゴールやビジョンに沿ったアクションを見直す
  - プロジェクトリストを見返し、目標や結果の状態を一つ一つ評価する
  - 各項目について少なくとも1つの次の取るべき行動があることを確認する
- [ ] いつかやるリストの更新
  - [ ] =C-c n f= cancel で 中断中のプロジェクトを見返す
** 閉じる
- [ ] org-agenda タスクをアーカイブする
"
          :if-new (file+head ,file-path ,(format "#+title: %s\n" title))
          :immediate-finish t
          :unnarrowed t))))

  (setq org-roam-capture-ref-templates
    '(
       ("r" "ref" plain
         "%?"
         :target (file+head "resources/ref/%<%Y%m%d%H%M%S>-${slug}.org"
                   "#+title: ${title}\n${ref}\n${body}")
         :unnarrowed t)
       ("z" "rez (Resonance Calendar) from protocol" plain "%?"
         :target (file+head "resources/rez/%<%Y%m%d%H%M%S>-${slug}.org"
                   "#+title: ${title}
* ${title}
:PROPERTIES:
:Type: ${type}
:Start: <%<%Y-%m-%d %a>>
:Fin:
:Canceled:
:Rating:
:Creator: ${creator}
:URL: ${ref}
:ReleaseDate: ${releaseDate}
:END:

** Why
# なぜ読もう・見ようと思ったのか

** Tasks
*** TODO 読む・見る

** Key Ideas

** Review

** Quotes

** Notes

")
         :unnarrowed t)
       ))

  (defun my/org-get-title-from-file (file)
    "Extracts the #+TITLE from the org FILE, or uses the filename if none found."
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
          (match-string 1)
        (file-name-nondirectory file))))

  (defun my/org-list-files-in-directory (dir)
    "Lists all org files in the given directory DIR within org-roam-directory."
    (directory-files (expand-file-name dir org-roam-directory)
                     t "\\.org$"))

  (defun my/org-open-file-main-window (file)
    "Open FILE in the main window."
    (let ((main-window (get-largest-window)))
      (select-window main-window)
      (find-file file)))

  (defun my/org-file-link-action (file)
    "Custom action for org file links to open in main window."
    (my/org-open-file-main-window file))

  (defun my/org-generate-buffer-from-files (files dir)
    "Create a buffer that lists FILES with their titles as links, and display it in a side window.
DIR is the directory name for display purposes."
    (let* ((buf (get-buffer-create "*Org-Roam Files*"))
           (window (display-buffer-in-side-window
                    buf
                    '((side . left)
                      (slot . 0)
                      (window-width . 30)
                      (preserve-size . (t . nil))))))
      (with-current-buffer buf
        (erase-buffer)
        (org-mode)
        (org-link-set-parameters "orgfile"
                                 :follow #'my/org-file-link-action)
        (insert (format "Files in %s\n\n" dir))
        (dolist (file files)
          (let ((title (my/org-get-title-from-file file)))
            (insert (format "- [[orgfile:%s][%s]]\n" file title))))
        (goto-char (point-min))
        (read-only-mode 1))
      ;; サイドウィンドウにフォーカスを移動
      (select-window window)))

  (defun my/org-open-areas-files ()
    "Opens a list of the org files in the 'areas' directory."
    (interactive)
    (let ((files (my/org-list-files-in-directory "areas")))
      (my/org-generate-buffer-from-files files "areas")))

  (defun my/org-open-projects-files ()
    "Opens a list of the org files in the 'projects' directory."
    (interactive)
    (let ((files (my/org-list-files-in-directory "projects")))
      (my/org-generate-buffer-from-files files "projects")))
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

(setq markdown-fontify-code-blocks-natively t)
