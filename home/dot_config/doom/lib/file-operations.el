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

;;; :ARCHIVE: サブツリー抽出

(defun my/--archive-tree-insert (node root-p)
  "NODE のツリーをカレントバッファにorg形式で出力する。
NODE は (heading-line children... (:subtree . text)...) の形式。
ROOT-P が non-nil なら見出し行自体は出力しない（仮想ルート）。"
  (unless root-p
    (insert (car node) "\n"))
  (dolist (child (cdr node))
    (cond
     ((and (consp child) (eq (car child) :subtree))
      (insert (cdr child))
      (unless (bolp) (insert "\n")))
     ((listp child)
      (my/--archive-tree-insert child nil)))))

(defun my/--archive-build-content (entries)
  "ENTRIES から親見出しの階層を保ったorg文字列を生成する。
ENTRIES は ((parent-lines subtree-text) ...) のリスト。"
  (let ((root (list :root)))
    (dolist (entry entries)
      (let ((parents (car entry))
            (subtree (cadr entry))
            (node root))
        ;; 親見出しのパスをたどり、なければ作る
        (dolist (p parents)
          (let ((child (cl-find-if (lambda (c)
                                     (and (listp c)
                                          (not (eq (car c) :subtree))
                                          (equal (car c) p)))
                                   (cdr node))))
            (unless child
              (setq child (list p))
              (nconc node (list child)))
            (setq node child)))
        ;; サブツリーをこのノードに追加
        (nconc node (list (cons :subtree subtree)))))
    (with-temp-buffer
      (my/--archive-tree-insert root t)
      (buffer-string))))

(defun my/org-extract-archive-subtrees ()
  "カレントorgファイルの :ARCHIVE: タグ付きサブツリーを別ファイルに抽出する。
抽出先: `org-directory'/archives/(ファイル名)-(日付).org
親の見出し階層は抽出先でも維持される。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode バッファではありません"))
  (let* ((src-file (buffer-file-name))
         (base-name (file-name-sans-extension (file-name-nondirectory src-file)))
         (date-str (format-time-string "%Y%m%d"))
         (archive-dir (expand-file-name "archives/" org-directory))
         (archive-file (expand-file-name
                        (format "%s-%s.org" base-name date-str) archive-dir))
         (entries '())
         (markers '()))
    ;; 同名ファイルが既にある場合は連番を付ける
    (let ((n 1))
      (while (file-exists-p archive-file)
        (setq archive-file
              (expand-file-name
               (format "%s-%s_%d.org" base-name date-str n) archive-dir))
        (cl-incf n)))
    ;; Phase 1: :ARCHIVE: サブツリーを収集
    (org-map-entries
     (lambda ()
       (let* ((m (point-marker))
              (beg (point))
              (end (save-excursion (org-end-of-subtree t) (point)))
              (subtree (buffer-substring-no-properties beg end))
              (parents '()))
         (save-excursion
           (while (org-up-heading-safe)
             (push (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))
                   parents)))
         (push m markers)
         (push (list parents subtree) entries)))
     "ARCHIVE" 'file)
    (setq entries (nreverse entries))
    (setq markers (nreverse markers))
    (if (null entries)
        (message ":ARCHIVE: タグ付きサブツリーが見つかりません")
      ;; Phase 2: アーカイブファイルに書き出し
      (mkdir archive-dir t)
      (let ((content (my/--archive-build-content entries)))
        (with-temp-file archive-file
          (insert content)))
      ;; Phase 3: 元ファイルから削除（後ろから順に削除して位置ずれを防ぐ）
      (dolist (m (reverse markers))
        (goto-char m)
        (let ((beg (point)))
          (org-end-of-subtree t)
          (when (and (< (point) (point-max))
                     (looking-at "\n"))
            (forward-char 1))
          (delete-region beg (point)))
        (set-marker m nil))
      (save-buffer)
      (message "%d 個の :ARCHIVE: サブツリーを抽出しました: %s"
               (length entries) archive-file))))

(provide 'file-operations)
;;; file-operations.el ends here