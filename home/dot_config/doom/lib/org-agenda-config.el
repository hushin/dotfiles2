;;; $DOOMDIR/org-agenda-config.el -*- lexical-binding: t; -*-

;; org-agenda の設定

(defun my/org--agenda-files-in (dir)
  "DIR（`org-roam-directory' からの相対パス）配下の org ファイルを返す。"
  (let ((path (expand-file-name dir org-roam-directory)))
    (when (file-directory-p path)
      (directory-files-recursively path "\\.org$"))))

(defun my/org--default-agenda-files ()
  "notes の meta/config.org の既定値に対応するアジェンダ対象ファイル。
リポジトリ直下の *.org と gtd/ areas/ projects/ posts/ resources/ 配下。"
  (append
    (when (file-directory-p org-directory)
      (seq-filter #'file-regular-p
                  (directory-files org-directory t "\\.org$")))
    (my/org--agenda-files-in "gtd")
    (my/org--agenda-files-in "areas")
    (my/org--agenda-files-in "projects")
    (my/org--agenda-files-in "posts")
    (my/org--agenda-files-in "resources")))

(defun my/org--set-agenda-files ()
  "アジェンダ対象を再計算して `org-agenda-files' に設定する。"
  (setq my-default-org-agenda-files (my/org--default-agenda-files))
  (setq org-agenda-files my-default-org-agenda-files))

;; `custom.el' は config.el の後に読まれる。Customize が保存した古い
;; org-agenda-files（Dropbox 時代のパス）を起動完了後に上書きする。
(add-hook 'after-init-hook #'my/org--set-agenda-files)

(defconst my/org-focus-tag "@focus"
  "「次やること」の対象ファイルにつけるファイルタグ。
`org-use-tag-inheritance' が \"^@\" なので、見出しへ継承されるよう @ で始める。")

(defun my/org--focus-file-p ()
  "今のバッファの #+filetags に `my/org-focus-tag' があれば non-nil。"
  (member my/org-focus-tag
          (split-string (or (cadr (assoc "FILETAGS"
                                         (org-collect-keywords '("filetags"))))
                            "")
                        ":" t)))

(defun my/org-toggle-focus ()
  "今のファイルを「次やること」の対象にする／外す（#+filetags の付け外し）。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org ファイルではありません"))
  (save-excursion
    (goto-char (point-min))
    (if (my/org--focus-file-p)
        (progn (org-roam-tag-remove (list my/org-focus-tag))
               (message "Focus から外しました"))
      (org-roam-tag-add (list my/org-focus-tag))
      (message "Focus に入れました")))
  ;; org-roam の DB は保存時に更新されるので、`my/org-focus-find' にすぐ出るよう保存する
  (save-buffer))

(defun my/org-focus-find ()
  "`my/org-focus-tag' のついたファイルを選んで開く。"
  (interactive)
  (org-roam-node-visit
   (org-roam-node-read nil
                       (lambda (node)
                         (and (= (org-roam-node-level node) 0)
                              (member my/org-focus-tag (org-roam-node-tags node))))
                       nil t "Focus: ")))

(map! :leader
      :desc "Toggle focus file" "n p" #'my/org-toggle-focus
      :desc "Find focus file" "n P" #'my/org-focus-find)

(defun my/org-agenda--file-title (item)
  "agenda の ITEM があるファイルの #+title（無ければバッファ名）。"
  (when-let ((m (get-text-property 0 'org-marker item)))
    (with-current-buffer (marker-buffer m)
      (or (vulpea-buffer-prop-get "title") (buffer-name)))))

(defun my/org-agenda--done-ancestor-p ()
  "point の見出しの祖先に DONE / CANCELED があれば non-nil。"
  (save-excursion
    (cl-loop while (org-up-heading-safe)
             thereis (member (org-get-todo-state) '("DONE" "CANCELED")))))

(defun my/org-agenda-archive-done-tasks (&optional _match)
  "アジェンダ対象の DONE / CANCELED タスクを archive sibling へ移す。
`C-c a t 3 r * x A' の代わり。件数を確認してから実行する。
祖先も DONE / CANCELED の見出しは親と一緒に移るので数えない。"
  (interactive)
  (let ((markers
         (delq nil
               (org-map-entries
                (lambda ()
                  (unless (my/org-agenda--done-ancestor-p)
                    ;; 直前に Archive 見出しが挿入されても、この見出しを指し続ける
                    (copy-marker (point) t)))
                "/DONE|CANCELED" 'agenda 'archive))))
    (cond
     ((null markers)
      (message "アーカイブする DONE / CANCELED タスクはありません"))
     ((y-or-n-p (format "%d 件の DONE / CANCELED タスクをアーカイブしますか？"
                        (length markers)))
      (dolist (m markers)
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char m)
              (org-archive-to-archive-sibling))))
        (set-marker m nil))
      (org-save-all-org-buffers)
      (message "%d 件をアーカイブしました" (length markers)))
     (t
      (mapc (lambda (m) (set-marker m nil)) markers)))))

(after! org
  (my/org--set-agenda-files)

  ;; org-roam で作ったファイルの category 表示をいい感じにする
  ;; refs. https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
  (setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 18)%?-18t% s")
        (todo . " %i %(vulpea-agenda-category 18) ")
        (tags . " %i %(vulpea-agenda-category 18) ")
        (search . " %i %(vulpea-agenda-category 18) ")))
  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name))))
            (title (vulpea-buffer-prop-get "title"))
            (category (org-get-category))
            (result
              (or (if (and
                        title
                        (string-equal category file-name))
                    title
                    category)
                "")))
      (if (numberp len)
        (let* ((truncated (truncate-string-to-width result (- len 3) 0 ?\s "..."))
                (width (string-width truncated))
                (padded (concat truncated (make-string (- len width) ?\s))))
          padded)
        result)))
  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
              (point-max) t)
        (buffer-substring-no-properties
          (match-beginning 1)
          (match-end 1)))))

  (setq
    ;; org-agenda-skip-scheduled-if-done t
    ;; org-agenda-skip-deadline-if-done t
    ;; org-agenda-include-deadlines t
    ;; org-agenda-block-separator nil
    ;; org-agenda-compact-blocks t
    ;; org-agenda-start-with-log-mode t
    org-agenda-start-day nil)

  (setq org-agenda-custom-commands
    '(
       ("A" "Archive DONE/CANCELED tasks" my/org-agenda-archive-done-tasks "")
       ("dt" "Done today"
         ((agenda ""
            (
              (org-agenda-span 'day)
              (org-agenda-log-mode-items '(closed clock))
              (org-agenda-show-log t)
              ; nil にしないと org-element-at-point の Warning が出る
              (org-agenda-use-time-grid nil)
              (org-super-agenda-groups
                '(
                   (:name "Clocked today" :log t)
                   (:discard (:anything t))
                   ))
              )))
         )
       ("dw" "Done this week"
         ((agenda ""
            (
              (org-agenda-span 7)
              (org-agenda-start-on-weekday 1) ; 月曜日開始
              (org-agenda-log-mode-items '(closed clock))
              (org-agenda-show-log t)
              ; nil にしないと org-element-at-point の Warning が出る
              (org-agenda-use-time-grid nil)
              (org-super-agenda-groups
                '(
                   (:name "Clocked today" :log t)
                   (:discard (:anything t))
                   ))
              )))
         )
       ("a" "Default agenda"
         (
           (todo "NEXT" ((org-agenda-overriding-header "\nNEXT")))
           ;; my/org-toggle-focus で印をつけたファイルのタスクをファイルごとに出す
           (tags-todo "@focus"
             ((org-agenda-overriding-header "\nFocus")
               (org-agenda-hide-tags-regexp "^@focus$")
               (org-super-agenda-groups
                 '((:auto-map my/org-agenda--file-title)))))
           ;; TODO 会議など、今日の calendar 表示
           (agenda ""
            (
              (org-agenda-span 'day)
              (org-super-agenda-groups
                '(
                   (:name "Today"
                     :scheduled today
                     :deadline today)
                   (:name "Overdue"
                     :deadline past)
                   (:name "Reschedule"
                     :scheduled past)
                   ;; (:name "Due Soon"
                   ;;   :deadline future
                   ;;   :scheduled future                     ;; なぜか 将来の scheduled が表示されない
                   ;;   )
                   (:discard (:anything t))
                   ))))
           (tags-todo (concat "w" (format-time-string "%V"))
             ((org-agenda-overriding-header  (concat "ToDos Week " (format-time-string "%V")))
               (org-super-agenda-groups
                 '((:discard (:deadline t))
                    (:discard (:scheduled t))
                    (:discard (:todo ("DONE")))
                    ))))
           (agenda ""
             (
               (org-agenda-start-day "+1d")
               (org-agenda-span 7)
               (org-agenda-show-log nil)
               (org-agenda-clockreport-mode nil)))
           (todo "WAITING" ((org-agenda-overriding-header "\nWAITING")))
           (alltodo ""
             (
               (org-agenda-overriding-header "Tasks")
               (org-super-agenda-groups
                 '(
                    (:name "Important"
                      :tag "Important"
                      :priority "A"
                      )
                    (:name "Next Action"
                      :category "Next Action"
                      :priority "B")
                    (:name "Shopping"
                      :category "Shopping")
                    (:name "Inbox"
                      :category "Inbox")
                    (:discard (:anything t))
                  )
               ))
           ))
       )
       ("W" "for Weekly review"
         (
           (todo "NEXT" ((org-agenda-overriding-header "\nNEXT")))
           (agenda ""
             (
               (org-agenda-span 12)
               (org-agenda-show-log nil)
               (org-agenda-clockreport-mode nil)))
           (todo "WAITING" ((org-agenda-overriding-header "\nWAITING")))
           (alltodo ""
             (
               (org-agenda-files my-default-org-agenda-files)
               (org-agenda-overriding-header "Tasks")
               (org-super-agenda-groups
                 '(
                    (:name "Important"
                      :tag "Important"
                      :priority "A"
                      )
                    (:name "Next Action"
                      :category "Next Action"
                      :priority "B")
                    (:name "Shopping"
                      :category "Shopping")
                    (:name "Projects"
                      :file-path "projects/")
                    (:name "Areas"
                      :file-path "areas/")
                    (:name "Resources"
                      :file-path "resources/")
                    (:name "Inbox"
                      :category "Inbox")
                    ))
               ))
           ))
       ("w" "All Weeks"
         ((tags-todo "{^w[0-9][0-9]$}"
            ((org-agenda-overriding-header "Tasks by ISO Week Number")
              (org-super-agenda-groups
                '((:auto-tags t)))))))
       ("z" "Zettelkasten tasks"
         ((alltodo ""
            ((org-agenda-files
               (directory-files-recursively
                 (concat (file-name-as-directory org-roam-directory) "zk")
                 "\\.org$"))
             (org-agenda-overriding-header "Zettelkasten Tasks")
             (org-super-agenda-groups
               '((:auto-category t)))))))
       ("b" "blog draft posts"
         (
           (alltodo ""
             (
               (org-agenda-files
                 (directory-files-recursively
                   (concat (file-name-as-directory org-roam-directory) "posts")
                   "\\.org$"))
               (org-agenda-overriding-header "Blog Draft")
               ))
           ))
       )
    )
  )
