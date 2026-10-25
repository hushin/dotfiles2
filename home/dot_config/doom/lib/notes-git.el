;;; lib/notes-git.el -*- lexical-binding: t; -*-
;;; Commentary:
;; notes リポジトリ専用の Git 自動同期。
;;
;; - 起動時と 5 分ごとに `git pull --rebase'
;; - 保存から 1 秒後に自動コミット（メッセージは "emacs: <path>"）
;; - 保存後 90 秒アイドル、または 5 分ごとに push
;; - 競合・push 失敗時は停止して通知する。自動解決はしない。
;;
;; 手動コマンド:
;;   M-x my/notes-git-sync-now   … 今すぐ pull（必要なら push）
;;   M-x my/notes-git-resume     … 停止状態から再開
;;   M-x my/notes-git-status     … 状態と最後のエラーを表示

;;; Code:

(require 'autorevert)
(require 'seq)
(require 'subr-x)

(defvar my/notes-git-pull-interval 300
  "定期 pull の間隔（秒）。")

(defvar my/notes-git-push-idle-delay 90
  "保存後、push までの無操作時間（秒）。")

(defvar my/notes-git-commit-delay 5
  "保存後、コミットまでの待ち時間（秒）。連続保存をまとめる。")

(defvar my/notes-git--queue nil)
(defvar my/notes-git--process nil)
(defvar my/notes-git--current nil)
(defvar my/notes-git--stopped nil)
(defvar my/notes-git--stop-reason nil)
(defvar my/notes-git--pending-push nil)
(defvar my/notes-git--push-timer nil)
(defvar my/notes-git--commit-timer nil)
(defvar my/notes-git--periodic-timer nil)

(defun my/notes-git--directory ()
  "自動同期する notes リポジトリのディレクトリ。"
  (file-name-as-directory (expand-file-name org-directory)))

(defun my/notes-git--truncate (str n)
  "STR を N 文字に切り詰める。"
  (truncate-string-to-width (or str "") n nil nil "..."))

(defun my/notes-git--notify (title body level)
  "TITLE / BODY を通知する。LEVEL は `display-warning' のレベル（:error など）。"
  (display-warning 'notes-git (concat title "\n\n" body) level)
  (message "notes-git: %s" title)
  (when (fboundp 'w32-notification-notify)
    (ignore-errors
      (w32-notification-notify
        :title (my/notes-git--truncate title 60)
        :body (my/notes-git--truncate body 250)
        :level (if (eq level :error) 'error 'warning)))))

(defun my/notes-git--cancel-push-timer ()
  (when (timerp my/notes-git--push-timer)
    (cancel-timer my/notes-git--push-timer))
  (setq my/notes-git--push-timer nil))

(defun my/notes-git--fail (step code stdout stderr)
  "STEP の失敗で自動同期を停止する。自動では再試行しない。"
  (setq my/notes-git--stopped t
        my/notes-git--stop-reason
        (format "git %s が失敗しました (exit %s)\n\n--- stderr ---\n%s\n--- stdout ---\n%s"
                step code
                (string-trim (or stderr ""))
                (string-trim (or stdout "")))
        my/notes-git--queue nil
        my/notes-git--current nil)
  (my/notes-git--cancel-push-timer)
  (when (timerp my/notes-git--commit-timer)
    (cancel-timer my/notes-git--commit-timer))
  (setq my/notes-git--commit-timer nil)
  (my/notes-git--notify
    "notes-git: 自動同期を停止しました"
    (concat
      (my/notes-git--truncate (string-trim (or stderr "")) 180)
      "\n\n"
      (format "確認: git -C \"%s\" status\n" (my/notes-git--directory))
      "競合を直したら `git rebase --continue' を実行し、"
      "`M-x my/notes-git-resume' で再開してください。")
    :error))

(defun my/notes-git--run (args callback)
  "notes リポジトリで git ARGS を非同期実行する。
CALLBACK は (EXIT-CODE STDOUT STDERR) を受ける。"
  (let* ((default-directory (my/notes-git--directory))
         (process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment))
         (stdout-buf (generate-new-buffer " *notes-git stdout*"))
         (stderr-buf (generate-new-buffer " *notes-git stderr*"))
         (proc (make-process
                 :name "notes-git"
                 :buffer stdout-buf
                 :stderr stderr-buf
                 :command (cons (or (executable-find "git") "git") args)
                 :coding (cons 'utf-8-unix 'utf-8-unix)
                 :connection-type 'pipe
                 :noquery t)))
    (setq my/notes-git--process proc)
    (set-process-sentinel
      proc
      (lambda (p _event)
        (when (memq (process-status p) '(exit signal))
          (let ((code (process-exit-status p))
                (out (with-current-buffer stdout-buf (buffer-string)))
                (err (with-current-buffer stderr-buf (buffer-string))))
            (ignore-errors (kill-buffer stdout-buf))
            (ignore-errors (kill-buffer stderr-buf))
            (setq my/notes-git--process nil)
            (condition-case error
                (funcall callback code out err)
              (error
               (my/notes-git--fail "内部エラー" nil
                                   (format "%S" error) "")))))))))

(defun my/notes-git--enqueue (action)
  "ACTION（pull / commit / push）をキューに積む。"
  (unless my/notes-git--stopped
    (unless (or (memq action my/notes-git--queue)
                (and (memq action '(pull push))
                     (eq action my/notes-git--current)))
      (setq my/notes-git--queue
            (append my/notes-git--queue (list action))))
    (my/notes-git--start-next)))

(defun my/notes-git--start-next ()
  (unless (or my/notes-git--process my/notes-git--stopped)
    (when-let ((action (pop my/notes-git--queue)))
      (setq my/notes-git--current action)
      (pcase action
        ('pull (my/notes-git--pull-flow))
        ('commit (my/notes-git--commit-flow #'my/notes-git--done))
        ('push (my/notes-git--push-flow))
        (_ (my/notes-git--done))))))

(defun my/notes-git--done ()
  (setq my/notes-git--current nil)
  (my/notes-git--start-next))

(defun my/notes-git--message-from-files (files)
  "FILES から \"emacs: <path>\" 形式のコミットメッセージを作る。"
  (let ((shown (seq-take files 3))
        (rest (- (length files) 3)))
    (concat "emacs: "
            (mapconcat #'identity shown ", ")
            (if (> rest 0) (format ", ... (%d files)" rest) ""))))

(defun my/notes-git--with-clean-tree (then)
  "作業ツリーに変更があればコミットしてから THEN を呼ぶ。"
  (my/notes-git--run
    '("status" "--porcelain")
    (lambda (code out err)
      (cond
        ((not (zerop code))
         (my/notes-git--fail "status" code out err))
        ((string-empty-p (string-trim out))
         (funcall then))
        (t
         (my/notes-git--commit-flow then))))))

(defun my/notes-git--commit-flow (then)
  "変更をコミットして THEN を呼ぶ。"
  (my/notes-git--run
    '("add" "-A" "--" "."
      ":(exclude)*~"
      ":(exclude)#*#"
      ":(exclude).#*"
      ":(exclude).DS_Store"
      ":(exclude)Thumbs.db")
    (lambda (code out err)
      (if (not (zerop code))
          (my/notes-git--fail "add" code out err)
        (my/notes-git--run
          '("diff" "--cached" "--name-only" "-z")
          (lambda (code out err)
            (if (not (zerop code))
                (my/notes-git--fail "diff --cached" code out err)
              (let ((files (split-string out "\0" t)))
                (if (null files)
                    (funcall then)
                  (let* ((message (my/notes-git--message-from-files files))
                         (msgfile (make-temp-file "notes-git-msg")))
                    ;; make-process は Windows で非 ASCII の引数を壊すため、
                    ;; コミットメッセージは UTF-8 のファイル経由で渡す。
                    (with-temp-file msgfile
                      (set-buffer-file-coding-system 'utf-8-unix)
                      (insert message "\n"))
                    (my/notes-git--run
                      (list "commit" "-F" msgfile)
                      (lambda (code out err)
                        (ignore-errors (delete-file msgfile))
                        (if (zerop code)
                            (progn
                              (setq my/notes-git--pending-push t)
                              (my/notes-git--schedule-push)
                              (funcall then))
                          (my/notes-git--fail "commit" code out err))))))))))))))

(defun my/notes-git--pull-flow ()
  (my/notes-git--with-clean-tree
    (lambda ()
      (my/notes-git--run
        '("pull" "--rebase")
        (lambda (code out err)
          (if (zerop code)
              (progn
                (my/notes-git--revert-buffers)
                (when my/notes-git--pending-push
                  (my/notes-git--schedule-push))
                (my/notes-git--done))
            (my/notes-git--fail "pull --rebase" code out err)))))))

(defun my/notes-git--push-flow ()
  (my/notes-git--with-clean-tree
    (lambda ()
      (let ((do-push
              (lambda ()
                (my/notes-git--run
                  '("push")
                  (lambda (code out err)
                    (if (zerop code)
                        (progn
                          (setq my/notes-git--pending-push nil)
                          (my/notes-git--cancel-push-timer)
                          (message "notes-git: push しました")
                          (my/notes-git--done))
                      (my/notes-git--fail "push" code out err)))))))
        (if (my/notes-git--modified-buffers-p)
            ;; 未保存の編集がある間は pull しない（画面の内容を上書きしない）
            (funcall do-push)
          (my/notes-git--run
            '("pull" "--rebase")
            (lambda (code out err)
              (if (zerop code)
                  (progn
                    (my/notes-git--revert-buffers)
                    (funcall do-push))
                (my/notes-git--fail "pull --rebase (push 前)" code out err)))))))))

(defun my/notes-git--modified-buffers-p ()
  "notes 配下に未保存のバッファがあるか。"
  (let ((dir (my/notes-git--directory)))
    (seq-some
      (lambda (buf)
        (when-let ((file (buffer-file-name buf)))
          (and (file-in-directory-p file dir)
               (buffer-modified-p buf))))
      (buffer-list))))

(defun my/notes-git--revert-buffers ()
  "notes 配下の未編集バッファをディスクの内容に合わせる。"
  (let ((dir (my/notes-git--directory))
        (reverted 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name
                   (file-in-directory-p buffer-file-name dir)
                   (not (buffer-modified-p)))
          (let ((tick (buffer-chars-modified-tick))
                (auto-revert-mode t))
            (auto-revert-handler)
            (when (/= tick (buffer-chars-modified-tick))
              (setq reverted (1+ reverted)))))))
    (when (> reverted 0)
      (message "notes-git: %d 個のバッファを読み直しました" reverted))))

(defun my/notes-git--schedule-commit ()
  (when (timerp my/notes-git--commit-timer)
    (cancel-timer my/notes-git--commit-timer))
  (setq my/notes-git--commit-timer
        (run-with-timer my/notes-git-commit-delay nil
                        #'my/notes-git--commit-now)))

(defun my/notes-git--commit-now ()
  (setq my/notes-git--commit-timer nil)
  (unless my/notes-git--stopped
    (my/notes-git--enqueue 'commit)))

(defun my/notes-git--schedule-push ()
  (when (and my/notes-git--pending-push
             (not my/notes-git--stopped)
             (not (timerp my/notes-git--push-timer)))
    (setq my/notes-git--push-timer
          (run-with-idle-timer my/notes-git-push-idle-delay nil
                               #'my/notes-git--push-now))))

(defun my/notes-git--push-now ()
  (my/notes-git--cancel-push-timer)
  (when (and my/notes-git--pending-push
             (not my/notes-git--stopped))
    (my/notes-git--enqueue 'push)))

(defun my/notes-git--periodic ()
  (unless my/notes-git--stopped
    (my/notes-git--enqueue 'pull)
    (when my/notes-git--pending-push
      (my/notes-git--push-now))))

(defun my/notes-git--after-save ()
  (when (and buffer-file-name
             (not my/notes-git--stopped)
             (file-in-directory-p buffer-file-name (my/notes-git--directory)))
    (my/notes-git--schedule-commit)))

(defun my/notes-git--operation-in-progress-p ()
  "rebase / merge の途中かどうか。"
  (let ((git-dir (expand-file-name ".git" (my/notes-git--directory))))
    (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
        (file-exists-p (expand-file-name "rebase-apply" git-dir))
        (file-exists-p (expand-file-name "MERGE_HEAD" git-dir)))))

;;;###autoload
(defun my/notes-git-sync-now ()
  "notes リポジトリを今すぐ pull する（未 push があれば push も）。"
  (interactive)
  (when my/notes-git--stopped
    (user-error "notes-git: 停止中です。M-x my/notes-git-resume で再開してください"))
  (my/notes-git--enqueue 'pull)
  (when my/notes-git--pending-push
    (my/notes-git--push-now))
  (message "notes-git: 同期を開始しました"))

;;;###autoload
(defun my/notes-git-resume ()
  "失敗で停止した自動同期を再開する。"
  (interactive)
  (when my/notes-git--process
    (user-error "notes-git: git を実行中です。終わるまで待ってください"))
  (when (my/notes-git--operation-in-progress-p)
    (user-error "notes-git: rebase/merge が進行中です。解決して git rebase --continue などを実行してください"))
  (setq my/notes-git--stopped nil
        my/notes-git--stop-reason nil)
  (my/notes-git--enqueue 'pull)
  (when my/notes-git--pending-push
    (my/notes-git--schedule-push))
  (message "notes-git: 自動同期を再開しました"))

;;;###autoload
(defun my/notes-git-status ()
  "自動同期の状態と最後のエラーを表示する。"
  (interactive)
  (let ((buf (get-buffer-create "*notes-git*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "directory: %s\n" (my/notes-git--directory)))
      (insert (format "stopped: %s\n" (if my/notes-git--stopped "yes" "no")))
      (insert (format "pending push: %s\n" (if my/notes-git--pending-push "yes" "no")))
      (insert (format "current: %s\n" (or my/notes-git--current "none")))
      (when my/notes-git--stop-reason
        (insert "\n" my/notes-git--stop-reason "\n")))
    (display-buffer buf)))

(defun my/notes-git-setup ()
  "notes リポジトリの自動同期を有効にする。"
  (interactive)
  (let ((dir (my/notes-git--directory)))
    (cond
      ((not (file-directory-p dir))
       (message "notes-git: %s が無いため自動同期を無効にします" dir))
      ((not (executable-find "git"))
       (message "notes-git: git が見つからないため自動同期を無効にします"))
      (t
       (when (timerp my/notes-git--periodic-timer)
         (cancel-timer my/notes-git--periodic-timer))
       (my/notes-git--cancel-push-timer)
       (when (timerp my/notes-git--commit-timer)
         (cancel-timer my/notes-git--commit-timer))
       (setq my/notes-git--commit-timer nil)
       (setq my/notes-git--periodic-timer
             (run-with-timer my/notes-git-pull-interval
                             my/notes-git-pull-interval
                             #'my/notes-git--periodic))
       (add-hook 'after-save-hook #'my/notes-git--after-save)
       (run-with-idle-timer 5 nil #'my/notes-git--periodic)
       (message "notes-git: %s の自動同期を有効にしました" dir)))))

(if (bound-and-true-p org-directory)
    (my/notes-git-setup)
  (message "notes-git: org-directory が未設定のため自動同期を有効にできません"))

(provide 'notes-git)
;;; notes-git.el ends here
