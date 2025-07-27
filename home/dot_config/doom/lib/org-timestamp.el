;;; org-timestamp.el --- Org mode timestamp functions -*- lexical-binding: t; -*-

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

;; Global keybinding for timestamp toggle
(global-set-key (kbd "C-c C-x t") #'my/org-mode-timestamp-toggle)

(provide 'org-timestamp)
;;; org-timestamp.el ends here