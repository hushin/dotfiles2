;;; lib/notes-archive.el -*- lexical-binding: t; -*-
;;; Commentary:
;; notes リポジトリのアーカイブを別ファイルへ逃がす。
;;
;; - アーカイブ先は `archives/<元パスから .org を除いたもの>_archive.org'
;;   例: projects/foo.org → archives/projects/foo_archive.org
;;       gtd.org          → archives/gtd_archive.org
;; - `org-mode-hook' で notes 配下の org ファイルの `org-archive-location' を
;;   そのファイルに向ける（C-c C-x C-s がそこへ移す）。
;; - archive sibling の入れ物 (`Archive :ARCHIVE:') に溜まった子見出しを、
;;   アーカイブファイルへ移して入れ物を片付ける。
;;
;; 手動コマンド:
;;   M-x my/notes-archive-flush-buffer … 今のバッファの入れ物を片付ける
;;   M-x my/notes-archive-flush-all    … notes 全体の入れ物を片付ける
;;
;; バッチ実行:
;;   emacs --batch -l notes-archive.el -f my/notes-archive-batch <notes-dir>
;;
;; Doom には依存しない。

;;; Code:

(require 'org)
(require 'org-archive)
(require 'cl-lib)
(require 'subr-x)

(defconst my/notes-archive--dir-name "archives"
  "アーカイブファイルを置くディレクトリ名（notes 直下）。")

(defconst my/notes-archive--container-regexp
  "^\\*+ \\(?:[^ \t\n]+[ \t]+\\)?Archive[ \t]+:\\(?:[^ \t\n:]+:\\)*ARCHIVE:\\(?:[^ \t\n:]+:\\)*[ \t\r]*$"
  "入れ物見出しの候補を拾う正規表現。最終判定は `my/notes-archive--container-p'。")

(defun my/notes-archive--notes-dir (&optional notes-dir)
  "NOTES-DIR（省略時は `org-directory'）を末尾スラッシュ付き絶対パスにする。"
  (file-name-as-directory (expand-file-name (or notes-dir org-directory))))

(defun my/notes-archive-file-for (file &optional notes-dir)
  "FILE のアーカイブファイルの絶対パスを返す。
NOTES-DIR（省略時は `org-directory'）配下で `archives/' 配下でない *.org 以外は nil。"
  (when (and file (string-suffix-p ".org" file))
    (let* ((dir (my/notes-archive--notes-dir notes-dir))
           (abs (expand-file-name file))
           (rel (and (string-prefix-p dir abs) (substring abs (length dir)))))
      (when (and rel
                 (not (string= rel ""))
                 (not (string-prefix-p (concat my/notes-archive--dir-name "/") rel)))
        (concat dir my/notes-archive--dir-name "/"
                (substring rel 0 (- (length ".org"))) "_archive.org")))))

;;;; ファイル作成

(defun my/notes-archive--title (source-buffer)
  "SOURCE-BUFFER の #+title: の値。無ければ拡張子を除いたファイル名。"
  (with-current-buffer source-buffer
    (or (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((case-fold-search t))
              (when (re-search-forward "^#\\+title:\\(.*\\)$" nil t)
                (let ((v (string-trim (match-string-no-properties 1)
                                      "[ \t\r\u3000]+" "[ \t\r\u3000]+")))
                  (unless (string-empty-p v) v))))))
        (file-name-base (or buffer-file-name (buffer-name))))))

(defun my/notes-archive--ensure-file (archive-file source-buffer)
  "ARCHIVE-FILE が無い（または空）なら #+title: 行だけで作る。
SOURCE-BUFFER は title の取得元。ARCHIVE-FILE を訪問するバッファを返す。"
  (make-directory (file-name-directory archive-file) t)
  (let ((buf (find-file-noselect archive-file)))
    (with-current-buffer buf
      (when (zerop (buffer-size))
        ;; format を通さず連結する（% のエスケープ不要）
        (insert "#+title: " (my/notes-archive--title source-buffer) " (archive)\n")
        (save-buffer)))
    buf))

(defvar my/notes-archive--current-dir nil
  "片付け中の notes ディレクトリ。nil なら `org-directory'。")

(defun my/notes-archive--around-archive (orig &rest args)
  "`org-archive-subtree' を包み、notes 配下ではアーカイブ先を初期化する。
`ARCHIVE_FILE' は環境ごとに違う絶対パスではなく notes からの相対パスにする。"
  (let* ((dir (my/notes-archive--notes-dir my/notes-archive--current-dir))
         (file buffer-file-name)
         (af (and file (my/notes-archive-file-for file dir))))
    (if (not af)
        (apply orig args)
      (when (string-prefix-p (concat af "::") org-archive-location)
        (my/notes-archive--ensure-file af (current-buffer)))
      (let ((rel (file-relative-name file dir))
            (put (symbol-function 'org-entry-put)))
        (cl-letf (((symbol-function 'org-entry-put)
                   (lambda (epom property value)
                     (funcall put epom property
                              (if (equal property "ARCHIVE_FILE") rel value)))))
          (apply orig args))))))

(advice-add 'org-archive-subtree :around #'my/notes-archive--around-archive)

;;;; 普段のアーカイブ

(defun my/notes-archive--setup-location ()
  "notes 配下の org バッファの `org-archive-location' を設定する。"
  (when-let* ((file buffer-file-name)
              (af (my/notes-archive-file-for file)))
    (setq-local org-archive-location (concat af "::"))))

(add-hook 'org-mode-hook #'my/notes-archive--setup-location)

;;;; 入れ物の片付け

(defun my/notes-archive--container-p ()
  "point の見出しが入れ物（見出しテキスト Archive + ARCHIVE タグ）なら non-nil。
CRLF が混在するファイルでは行末の CR のせいで org がタグを認識しないので、
見出し行の文字列で判定する。TODO キーワードは org-todo-keywords-1 にあるものだけ許す。"
  (and (org-at-heading-p)
       (save-excursion
         (beginning-of-line)
         (and (looking-at "\\*+ \\(?:\\([^ \t\n]+\\)[ \t]+\\)?Archive[ \t]+:\\(?:[^ \t\n:]+:\\)*ARCHIVE:\\(?:[^ \t\n:]+:\\)*[ \t\r]*$")
              (or (null (match-string 1))
                  (member (match-string 1) org-todo-keywords-1))))))

(defun my/notes-archive--inherit-except-archive (orig)
  "ORIG（`org-use-tag-inheritance' の値）から ARCHIVE タグを除く判定関数を返す。"
  (lambda (tag)
    (and (not (equal tag "ARCHIVE"))
         (cond ((functionp orig) (funcall orig tag))
               ((stringp orig) (string-match-p orig tag))
               ((listp orig) (and orig (member tag orig)))
               (t orig)))))

(defun my/notes-archive--archive-child (archive-file)
  "point の見出しを ARCHIVE-FILE へ移す。OLPATH は親の入れ物を含めない。"
  (let* ((orig-path (symbol-function 'org-get-outline-path))
         (org-archive-location (concat archive-file "::"))
         (org-archive-save-context-info '(file olpath category todo itags))
         (org-archive-file-header-format nil)
         (org-use-tag-inheritance
          (my/notes-archive--inherit-except-archive org-use-tag-inheritance)))
    (cl-letf (((symbol-function 'org-get-outline-path)
               (lambda (&rest args)
                 ;; 子の祖先パスの末尾は入れ物（Archive）なので落とす
                 (butlast (apply orig-path args)))))
      (org-archive-subtree))))

(defun my/notes-archive--flush-container (archive-file)
  "point の入れ物の子を ARCHIVE-FILE へ全部移し、入れ物を片付ける。
移した件数を返す。入れ物に本文があれば見出し行とプロパティだけ消して本文は残す。"
  (let ((marker (copy-marker (point)))
        (count 0))
    (while (progn (goto-char marker) (org-goto-first-child))
      (my/notes-archive--archive-child archive-file)
      (cl-incf count))
    (goto-char marker)
    (let* ((beg (line-beginning-position))
           (meta-end (save-excursion (org-end-of-meta-data t) (point)))
           (sub-end (save-excursion (org-end-of-subtree t t) (point)))
           (body (buffer-substring-no-properties (min meta-end sub-end) sub-end)))
      (if (string-match-p "[^ \t\n]" body)
          (delete-region beg (min meta-end sub-end))
        (delete-region beg sub-end)))
    (set-marker marker nil)
    count))

;;;###autoload
(defun my/notes-archive-flush-buffer (&optional notes-dir)
  "今のバッファの入れ物の子見出しをアーカイブファイルへ移す。
入れ物は文書順に処理し、空になった入れ物見出しは削除する。
移した件数を返す。入れ物が無ければバッファに触れない。"
  (interactive)
  (let ((af (my/notes-archive-file-for buffer-file-name notes-dir))
        (my/notes-archive--current-dir notes-dir)
        (count 0)
        (found nil))
    (unless af
      (user-error "notes 配下の（archives 以外の）org ファイルではありません"))
    (unless (derived-mode-p 'org-mode) (org-mode))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward my/notes-archive--container-regexp nil t)
          (beginning-of-line)
          (if (not (my/notes-archive--container-p))
              (end-of-line)
            (unless found
              (setq found t)
              (my/notes-archive--ensure-file af (current-buffer)))
            (cl-incf count (my/notes-archive--flush-container af))))))
    (when found
      (save-buffer)
      (when-let ((abuf (find-buffer-visiting af)))
        (with-current-buffer abuf (save-buffer))))
    (when (called-interactively-p 'interactive)
      (message "notes-archive: %d 件をアーカイブしました" count))
    count))

(defun my/notes-archive--org-files (notes-dir)
  "NOTES-DIR 配下の *.org を再帰的に列挙する（archives/ と . 始まりは除く）。"
  (let ((skip (concat notes-dir my/notes-archive--dir-name)))
    (directory-files-recursively
     notes-dir "\\.org\\'" nil
     (lambda (dir)
       (let ((d (directory-file-name dir)))
         (and (not (string-prefix-p "." (file-name-nondirectory d)))
              (not (string= d skip))))))))

(defun my/notes-archive--has-container-p (file)
  "FILE に入れ物らしい見出しがあるか（grep 相当）。"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward my/notes-archive--container-regexp nil t)))

;;;###autoload
(defun my/notes-archive-flush-all (&optional notes-dir)
  "NOTES-DIR（省略時 `org-directory'）配下の全入れ物を片付ける。
処理したファイル数と移した件数を (FILES . COUNT) で返す。"
  (interactive)
  (let* ((dir (my/notes-archive--notes-dir notes-dir))
         (files 0)
         (total 0))
    (dolist (file (my/notes-archive--org-files dir))
      (when (and (my/notes-archive-file-for file dir)
                 (not (file-symlink-p file))
                 (my/notes-archive--has-container-p file))
        (let* ((af (my/notes-archive-file-for file dir))
               (pre (find-buffer-visiting file))
               (apre (find-buffer-visiting af))
               (buf (or pre (find-file-noselect file))))
          (unwind-protect
              (let ((n (with-current-buffer buf
                         (my/notes-archive-flush-buffer dir))))
                (when (> n 0) (cl-incf files) (cl-incf total n)))
            (unless pre (kill-buffer buf))
            (unless apre
              (when-let ((ab (find-buffer-visiting af)))
                (kill-buffer ab)))))))
    (message "notes-archive: %d ファイル、%d 件をアーカイブしました" files total)
    (cons files total)))

;;;###autoload
(defun my/notes-archive-batch ()
  "バッチ入口。`emacs --batch -l notes-archive.el -f my/notes-archive-batch DIR'。"
  (let* ((dir (car command-line-args-left))
         (org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w/!)" "|" "CANCELED(c/!)")))
         (org-use-tag-inheritance "^@")
         (org-directory dir)
         (make-backup-files nil)
         (org-startup-folded nil))
    (unless dir (error "usage: -f my/notes-archive-batch <notes-dir>"))
    (setq command-line-args-left (cdr command-line-args-left))
    (my/notes-archive-flush-all dir)))

(provide 'notes-archive)
;;; lib/notes-archive.el ends here
