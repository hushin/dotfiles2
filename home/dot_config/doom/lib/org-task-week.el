;;; lib/org-task-week.el -*- lexical-binding: t; -*-


(defun my/org-week-info (delta)
  "今週からDELTA週ずれた週番号を返す。年またぎ考慮。"
  (let* ((now (decode-time))
          (dow (nth 6 now))
          (days-since-mon (if (= dow 0) 6 (- dow 1)))
          (this-monday (time-subtract (current-time)
                         (seconds-to-time (* days-since-mon 86400))))
          (target (time-add this-monday (seconds-to-time (* delta 7 86400)))))
    (string-to-number (format-time-string "%V" target))))


(defun my/org-shift-week-tags (from-week to-week)
  "org-agenda-files 全体で w%02d タグを FROM-WEEK から TO-WEEK に一括変換。
DONE/CANCELED は除外。"
  (let ((from-tag (format "w%02d" from-week))
         (to-tag (format "w%02d" to-week))
         (count 0))
    (org-map-entries
      (lambda ()
        (let ((todo (org-get-todo-state)))
          (when (not (member todo '("DONE" "CANCELED")))
            (org-toggle-tag from-tag 'off)
            (org-toggle-tag to-tag 'on)
            (setq count (1+ count)))))
      (format "w%02d-DONE-CANCELED" from-week) 'agenda)
    ;; 変更されたバッファを保存
    (dolist (file (org-agenda-files))
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (save-buffer)))))
    count))

(defun my/org--bulk-shift (from-delta to-delta action-label)
  (let* ((from-week (my/org-week-info from-delta))
          (to-week   (my/org-week-info to-delta))
          (total (my/org-shift-week-tags from-week to-week)))
    (message "[%s] 完了: w%02d → w%02d: %d件移動"
      action-label from-week to-week total)))

(defun my/org-carry-over-this-week ()
  "今週の未完了タスクを来週へ持ち越し"
  (interactive)
  (my/org--bulk-shift 0 1 "今週→来週"))

(defun my/org-pull-from-last-week ()
  "先週の未完了タスクを今週へ持ち越し"
  (interactive)
  (my/org--bulk-shift -1 0 "先週→今週"))


(defun my/org-generate-weekly-tags ()
  "今週から5週間後までの週番号タグ(w01-w53)を生成して設定します。"
  (let* ((current-week (string-to-number (format-time-string "%V")))
          (weeks-to-show 5)
          (tag-list '()))
    (dotimes (i (1+ weeks-to-show))
      (let* ((week (+ current-week i))
              ;; 53週を超える場合は1週に戻る計算
              (adjusted-week (if (> week 53) (- week 53) week))
              (tag-name (format "w%02d" adjusted-week)))
        (push (cons tag-name nil) tag-list)))
    (setq org-tag-alist (nreverse tag-list))))

;; 実行して適用
(my/org-generate-weekly-tags)
