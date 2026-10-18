;;; lib/org-task-week.el -*- lexical-binding: t; -*-

(defvar my/org-week-tag-future-weeks 5
  "現在週より先の何週分のタグを将来タスクとして残すか。
持ち越し判定（my/org--week-tag-past-p）と週番号タグ生成で共用する。")

(defun my/current-week-tag ()
  "現在の ISO 週番号タグ（例 \"w03\"）を返す。"
  (format "w%02d" (string-to-number (format-time-string "%V"))))

(defun my/org--week-tag-past-p (tag current-week)
  "TAG（例 \"w15\"）が CURRENT-WEEK より過去の週のタグかどうか。
現在週から my/org-week-tag-future-weeks 週先までのタグは nil を返す。"
  (and (string-match "\\`w\\([0-9][0-9]\\)\\'" tag)
    (let ((forward-distance (mod (- (string-to-number (match-string 1 tag)) current-week) 53)))
      (> forward-distance my/org-week-tag-future-weeks))))

(defun my/org--save-modified-agenda-buffers ()
  "編集済みの org-agenda-files のバッファを保存する。"
  (dolist (file (org-agenda-files))
    (when-let ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (when (buffer-modified-p)
          (save-buffer))))))


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
    (my/org--save-modified-agenda-buffers)
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
  "先週以前のすべての週のタグ(w01-w53)を今週のタグに付け替える。
DONE/CANCELED は除外。今週および my/org-week-tag-future-weeks 週先の
タグ（将来タスク）はそのまま残す。"
  (interactive)
  (let* ((current-week (string-to-number (format-time-string "%V")))
          (current-tag (format "w%02d" current-week))
          (count 0))
    (org-map-entries
      (lambda ()
        (let ((todo (org-get-todo-state)))
          (when (not (member todo '("DONE" "CANCELED")))
            (let ((changed nil))
              (dolist (tag (org-get-tags nil t))
                (when (my/org--week-tag-past-p tag current-week)
                  (org-toggle-tag tag 'off)
                  (setq changed t)))
                (when changed
                  (org-toggle-tag current-tag 'on)
                  (setq count (1+ count)))))))
      "{^w[0-9][0-9]$}" 'agenda)
    (my/org--save-modified-agenda-buffers)
    (message "[先週以前→今週] 完了: %s へ %d件移動" current-tag count)))


(defun my/org-generate-weekly-tags ()
  "今週から my/org-week-tag-future-weeks 週間後までの週番号タグ(w01-w53)を生成して設定します。"
  (let* ((current-week (string-to-number (format-time-string "%V")))
          (weeks-to-show my/org-week-tag-future-weeks)
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
