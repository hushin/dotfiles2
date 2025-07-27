;;; org-bullet-functions.el --- Functions to convert bullet points to org headings

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

(provide 'org-bullet-functions)
;;; org-bullet-functions.el ends here