;;; file-operations.el --- File manipulation functions and keybindings

;; ファイル移動
(defun move-buffer-file-to-directory ()
  "Move the current buffer file to a new directory, keeping the same file name."
  (interactive)
  (let* ((old-location (buffer-file-name))
         (file-name (file-name-nondirectory old-location))
         (new-dir (file-name-as-directory (expand-file-name (read-directory-name "Move to directory: ")))))
    (when (file-exists-p (concat new-dir file-name))
      (error "File '%s' already exists in directory '%s'!" file-name new-dir))
    (rename-file old-location (concat new-dir file-name) 1)
    (set-visited-file-name (concat new-dir file-name))
    (set-buffer-modified-p nil)
    (when (fboundp 'recentf-add-file)
      (recentf-add-file (concat new-dir file-name))
      (recentf-remove-if-non-kept old-location))
    (message "File moved from '%s' to '%s'" old-location (concat new-dir file-name))))

;; ファイル削除
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
        (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; アーカイブフォルダへ移動
(defun org-roam-move-file-to-archives ()
  "Move the current org-roam file to the archives directory, maintaining the directory structure, and open the moved file in a buffer."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (relative-path (file-relative-name file-path org-roam-directory))
         (archive-path (concat (file-name-as-directory org-roam-directory)
                               "archives/"
                               relative-path)))
    (when (and (org-roam-file-p)
               (not (string-prefix-p (concat (file-name-as-directory org-roam-directory)
                                             "archives/")
                                     file-path)))
      (mkdir (file-name-directory archive-path) t)
      (rename-file file-path archive-path)
      (set-visited-file-name archive-path)
      (set-buffer-modified-p nil)
      (when (fboundp 'recentf-add-file)
        (recentf-add-file archive-path)
        (recentf-remove-if-non-kept file-path))
      (message "File moved to archives: %s" archive-path))))

(defun org-roam-move-file-to-canceled-archives ()
  "Move the current org-roam file to the archives/canceled directory, maintaining the directory structure, and open the moved file in a buffer."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (relative-path (file-relative-name file-path org-roam-directory))
         (archive-path (concat (file-name-as-directory org-roam-directory)
                               "archives/canceled/"
                               relative-path)))
    (when (and (org-roam-file-p)
               (not (string-prefix-p (concat (file-name-as-directory org-roam-directory)
                                             "archives/canceled/")
                                     file-path)))
      (mkdir (file-name-directory archive-path) t)
      (rename-file file-path archive-path)
      (set-visited-file-name archive-path)
      (set-buffer-modified-p nil)
      (when (fboundp 'recentf-add-file)
        (recentf-add-file archive-path)
        (recentf-remove-if-non-kept file-path))
      (message "File moved to archives: %s" archive-path))))

;; キーバインディング
(map! :leader
  (:prefix "f"
    :desc "Move buffer file to directory" "m" #'move-buffer-file-to-directory)
  (:prefix "f"
    :desc "Delete file" "D" #'delete-file-and-buffer)
  (:prefix "f"
    :desc "archive file" "A" #'org-roam-move-file-to-archives)
  (:prefix "f"
    :desc "archive/cancel file" "Q" #'org-roam-move-file-to-canceled-archives)
  )

(provide 'file-operations)
;;; file-operations.el ends here