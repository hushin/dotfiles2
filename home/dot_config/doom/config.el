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
(when (winp)
  ;; Windows 用のクリップボード設定
  (set-selection-coding-system 'utf-16le-dos)
  ;; Windows search-project で 日本語で検索できるようにする
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
