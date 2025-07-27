;;; task-collection.el --- Task collection functions for org-agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for collecting and formatting tasks from org-agenda files.

;;; Code:


(defun my/get-title-or-filename (buffer file)
  "Get the Org file #+TITLE property or use the filename if title is nil."
  (with-current-buffer buffer
    (or (org-element-map (org-element-parse-buffer 'element) 'keyword
          (lambda (kw)
            (when (string= "TITLE" (org-element-property :key kw))
              (org-element-property :value kw)))
          nil t)
      (file-name-nondirectory file))))
(defun my/get-task-type (deadline scheduled todo-keyword)
  "タスクのタイプを決定する。
DEADLINE - 期限日付
SCHEDULED - 予定日付
TODO-KEYWORD - TODOキーワード"
  (cond
    ((string= todo-keyword "NEXT") "NEXT")
    (deadline
      (let* ((today (org-today))
              (diff (- deadline today)))
        (format "期限 %+d日" diff)))
    (scheduled
      (let* ((today (org-today))
              (diff (- scheduled today)))
        (format "予定 %+d日" diff)))
    (t "")))

(defun my/ensure-org-id (pom)
  "ポイントPOMにorg-idがなければ作成し、IDを返す"
  (or (org-id-get pom)
    (org-id-get-create pom)))

(defun my/format-task-link (id heading type file-title)
  "タスクへのリンクを整形する"
  (format "** TODO [[id:%s][%s]] %s : %s (%s)"
    id
    ""
    heading
    file-title
    type))

(defun my/collect-next-tasks-from-agenda-files (&optional days)
  "org-agenda-filesから条件に合うタスクを収集し、現在のバッファにリンクとして挿入する。
収集条件:
- todo-keywordがNEXT
- DUNEかCANCELED以外で、以下の条件のいずれか
- 期限が今日からX日後まで
- 予定が今日以前
Optional argument DAYS は期限のX日数（デフォルトは5）"
  (interactive "P")
  (let* ((days (or days 5))
          (tasks '())
          (today (org-today))
          (future-limit (+ today days)))

    ;; ファイルごとに処理
    (dolist (file (org-agenda-files))
      (let ((file-buffer (find-file-noselect file t))
              (file-title nil))
        (with-current-buffer file-buffer
          (setq file-title (my/get-title-or-filename file-buffer file))
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let* ((element (org-element-at-point))
                      (todo-keyword (org-element-property :todo-keyword element))
                      (deadline (org-element-property :deadline element))
                      (scheduled (org-element-property :scheduled element))
                      (heading (org-element-property :title element))
                      (deadline-date (when deadline
                                        (time-to-days
                                          (org-time-string-to-time
                                            (org-element-property :raw-value deadline)))))
                      (scheduled-date (when scheduled
                                        (time-to-days
                                          (org-time-string-to-time
                                            (org-element-property :raw-value scheduled))))))

                ;; 条件判定
                (when (or
                        ;; 条件1: todo-keywordがNEXT
                        (and todo-keyword (string= todo-keyword "NEXT"))
                        ;; 条件2: DONE/CANCELED以外で、期限がX日以内または予定が今日以前
                        (and todo-keyword
                          (not (or (string= todo-keyword "DONE")
                                  (string= todo-keyword "CANCELED")))
                          (or (and deadline-date (<= deadline-date future-limit))
                            (and scheduled-date (<= scheduled-date today)))))

                  ;; タスクの情報を収集
                  (let* ((id (my/ensure-org-id element))
                          (type (my/get-task-type deadline-date scheduled-date todo-keyword))
                          (task-data (list
                                        :link (my/format-task-link id heading type file-title)
                                        :is-next (and todo-keyword (string= todo-keyword "NEXT"))
                                        :scheduled-date scheduled-date
                                        :deadline-date deadline-date
                                        :has-scheduled (not (null scheduled-date))
                                        :has-deadline (not (null deadline-date)))))
                    (push task-data tasks)))))))))

    ;; タスクの並び替え (NEXT→予定→期限の順、日付は過去から未来へ)
    (setq tasks
      (sort tasks
        (lambda (a b)
          (cond
            ;; NEXTを最優先
            ((and (plist-get a :is-next) (not (plist-get b :is-next))) t)
            ((and (not (plist-get a :is-next)) (plist-get b :is-next)) nil)

            ;; 次に、予定(scheduled)があるかどうかで分類
            ((and (plist-get a :has-scheduled) (not (plist-get b :has-scheduled))) t)
            ((and (not (plist-get a :has-scheduled)) (plist-get b :has-scheduled)) nil)

            ;; 両方予定があれば日付で比較（過去順）
            ((and (plist-get a :has-scheduled) (plist-get b :has-scheduled))
              (< (plist-get a :scheduled-date) (plist-get b :scheduled-date)))

            ;; 次に、期限(deadline)があるかどうかで分類
            ((and (plist-get a :has-deadline) (not (plist-get b :has-deadline))) t)
            ((and (not (plist-get a :has-deadline)) (plist-get b :has-deadline)) nil)

            ;; 両方期限があれば日付で比較（過去順）
            ((and (plist-get a :has-deadline) (plist-get b :has-deadline))
              (< (plist-get a :deadline-date) (plist-get b :deadline-date)))

            ;; それ以外の場合
            (t nil)))))

    ;; 収集したタスクを現在のバッファに挿入
    (when tasks
      (insert (mapconcat (lambda (task) (plist-get task :link)) tasks "\n"))
      (insert "\n"))

    ;; メッセージ表示
    (message "%d個のタスクを挿入しました" (length tasks))))


(provide 'task-collection)
;;; task-collection.el ends here
