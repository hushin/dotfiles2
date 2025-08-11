;;; $DOOMDIR/org-agenda-config.el -*- lexical-binding: t; -*-

;; org-agenda の設定

(after! org
  ;; agenda
  (setq my-default-org-agenda-files
    (append
      (directory-files org-directory t "\\.org$")
      (directory-files-recursively (concat org-roam-directory "areas") "\\.org$")
      (directory-files-recursively (concat org-roam-directory "projects") "\\.org$")
      (directory-files-recursively (concat org-roam-directory "resources") "\\.org$")))

  (setq org-agenda-files my-default-org-agenda-files)

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

  (defun cmp-date-property-stamp (prop)
    "Compare two `org-mode' agenda entries, `A' and `B', by some date property.
If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
    ;; source: https://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda/26369#26369
    (lambda (a b)
      (let* ((a-pos (get-text-property 0 'org-marker a))
              (b-pos (get-text-property 0 'org-marker b))
              (a-date (or (org-entry-get a-pos prop)
                        (format "<%s>" (org-read-date t nil "now"))))
              (b-date (or (org-entry-get b-pos prop)
                        (format "<%s>" (org-read-date t nil "now"))))
              (cmp (compare-strings a-date nil nil b-date nil nil)))
        (if (eq cmp t) nil (cl-signum cmp)))))
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
       ("r" "Resonance Cal" tags "Type={.}"
         ((org-agenda-files
            (directory-files-recursively
              (concat org-roam-directory "resources/rez") "\\.org$"))
           (org-overriding-columns-format
             "%35Item %Type %Start %Fin %Rating")
           (org-agenda-cmp-user-defined
             (cmp-date-property-stamp "Start"))
           (org-agenda-sorting-strategy
             '(user-defined-down))
           (org-agenda-overriding-header "C-u r to re-run Type={.}")
           (org-agenda-view-columns-initially t)
           )
         )
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
           (agenda ""
             (
               (org-agenda-start-day "+1d")
               (org-agenda-span 7)
               (org-agenda-show-log nil)
               (org-agenda-clockreport-mode nil)))
           (todo "WAITING" ((org-agenda-overriding-header "\nWAITING")))
           ;; (tags (concat "w" (format-time-string "%V"))
           ;;   ((org-agenda-overriding-header  (concat "ToDos Week " (format-time-string "%V")))
           ;;     (org-super-agenda-groups
           ;;       '((:discard (:deadline t))
           ;;          (:discard (:scheduled t))
           ;;          (:discard (:todo ("DONE")))
           ;;          ))))
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
       ("w" "for Weekly review"
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
               (org-agenda-files
                 (append my-default-org-agenda-files
                   (directory-files-recursively (concat org-roam-directory "zk") "\\.org$")))
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
                    (:name "Zettelkasten"
                      :file-path "zk/")
                    (:name "Inbox"
                      :category "Inbox")
                    ))
               ))
           ))
       )
    )
  )
