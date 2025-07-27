;;; org-file-browser.el --- Org file browser functions -*- lexical-binding: t; -*-

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

(provide 'org-file-browser)
;;; org-file-browser.el ends here