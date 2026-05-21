;;; org-prompt-path.el --- Org prompt path helpers -*- lexical-binding: t; -*-

(defconst my/org-prompt-path-fallback-ignored-directories
  '(".git" ".hg" ".svn" "node_modules" ".direnv" ".next" ".nuxt" ".turbo"
    ".yarn" ".pnpm-store" "dist" "build" ".eln-cache")
  "Git が使えない場合に探索対象から外すディレクトリ名。")

(defun my/org-set-prompt-base-dir (dir)
  "現在の見出しの PROMPT_BASE_DIR プロパティに DIR を設定する。
カーソル位置の見出し（または親見出し）にプロパティを書き込む。"
  (interactive
   (list (read-directory-name "Base dir: "
                              (or (org-entry-get nil "PROMPT_BASE_DIR" t)
                                  default-directory))))
  (org-set-property "PROMPT_BASE_DIR" (expand-file-name dir)))

(defun my/org--prompt-base-dir ()
  "現在の見出しから継承された PROMPT_BASE_DIR を返す。"
  (let ((base (org-entry-get nil "PROMPT_BASE_DIR" t)))
    (unless base
      (user-error "PROMPT_BASE_DIR プロパティが見つかりません。先に my/org-set-prompt-base-dir で設定してください"))
    (expand-file-name base)))

(defun my/org--git-root (dir)
  "DIR を含む Git ルートを返す。見つからなければ nil。"
  (let ((root (locate-dominating-file dir ".git")))
    (when root
      (expand-file-name root))))

(defun my/org--git-ls-files (base)
  "BASE 配下の Git 非無視ファイル一覧を相対パスで返す。"
  (when-let* ((git (executable-find "git"))
              (git-root (my/org--git-root base)))
    (let* ((default-directory git-root)
           (relative-base (file-relative-name base git-root))
           (args (append (list git nil t nil
                               "ls-files" "--cached" "--others" "--exclude-standard" "-z")
                         (unless (string= relative-base "./")
                           (list "--" relative-base))))
           (output (with-temp-buffer
                      (if (zerop (apply #'process-file args))
                          (buffer-string)
                        ""))))
      (sort
       (mapcar (lambda (file)
                 (file-relative-name (expand-file-name file git-root) base))
               (split-string output "\0" t))
       #'string<))))

(defun my/org--fallback-directory-filter (dir)
  "DIR を再帰探索するかどうかを返す。"
  (not (member (file-name-nondirectory (directory-file-name dir))
               my/org-prompt-path-fallback-ignored-directories)))

(defun my/org--fallback-files (base)
  "BASE 配下の代表的な不要ディレクトリを除外してファイル一覧を返す。"
  (let ((files (mapcar (lambda (file)
                         (file-relative-name file base))
                        (directory-files-recursively
                         base
                         "."
                         nil
                         #'my/org--fallback-directory-filter))))
    (sort files #'string<)))

(defun my/org--visible-files (base)
  "BASE 配下で補完候補に出すファイル一覧を返す。"
  (or (my/org--git-ls-files base)
      (my/org--fallback-files base)))

(defun my/org--read-relative-path-fuzzy (base)
  "BASE 配下のファイルを曖昧補完で選ばせ、相対パスを返す。"
  (let ((files (my/org--visible-files base)))
    (unless files
      (user-error "PROMPT_BASE_DIR 配下にファイルがありません: %s" base))
    (completing-read (format "ファイル (%s): " (abbreviate-file-name base))
                     files
                     nil
                     t
                     nil
                     nil)))

(defun my/org--insert-relative-path (file base)
  "BASE 基準の FILE の相対パスを挿入する。"
  (insert (file-relative-name (expand-file-name file) base)))

(defun my/org-insert-relative-path (file)
  "PROMPT_BASE_DIR 基準で .gitignore を考慮した曖昧補完から相対パスを挿入する。"
  (interactive
   (let ((base (my/org--prompt-base-dir)))
     (list (expand-file-name (my/org--read-relative-path-fuzzy base) base))))
  (let ((base (my/org--prompt-base-dir)))
    (my/org--insert-relative-path file base)))

(defun my/org-insert-relative-path-by-traversal (file)
  "PROMPT_BASE_DIR 基準でディレクトリを辿って相対パスを挿入する。"
  (interactive
   (let ((base (my/org--prompt-base-dir)))
     (list (read-file-name "ファイル: " base nil t))))
  (let ((base (my/org--prompt-base-dir)))
    (my/org--insert-relative-path file base)))

(provide 'org-prompt-path)
;;; org-prompt-path.el ends here
