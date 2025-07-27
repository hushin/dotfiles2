;;; weekly-review.el --- Weekly review functions -*- lexical-binding: t; -*-

(defun my/create-weekly-review ()
  "Create a new org-roam file for the current week's review."
  (interactive)
  (let* ((week-number (format-time-string "%V"))
          (year (format-time-string "%Y"))
          (slug (concat year "w" week-number))
          (file-path (format "weekly/%s.org" slug))
          (title (format "Weekly Review %sw%s" year week-number)))
    (my/org-roam-capture-weekly title file-path)))

(defun my/create-previous-weekly-review ()
  "Create a new org-roam file for the previous week's review."
  (interactive)
  (let* ((week-number (string-to-number (format-time-string "%V")))
          (year (string-to-number (format-time-string "%Y")))
          (previous-week (if (= week-number 1)
                           52
                           (1- week-number)))
          (adjusted-year (if (= week-number 1)
                           (1- year)
                           year))
          (slug (concat (number-to-string adjusted-year) "w" (format "%02d" previous-week)))
          (file-path (format "weekly/%s.org" slug))
          (title (format "Weekly Review %sw%s" adjusted-year (format "%02d" previous-week))))
    (my/org-roam-capture-weekly title file-path)))

(defun my/org-roam-capture-weekly (title file-path)
  "Helper function to capture a weekly review in org-roam."
  (org-roam-capture-
    :node (org-roam-node-create :title title)
    :props '(:finalize find-file)
    :templates
    `(("w" "weekly review" plain
        "* Review
レビュー日 <%<%Y-%m-%d %a>>
** 今週振り返り
# 日記を読み返して振り返る
*** Good
*** Problem
*** 来週達成したいこと
** 明確にする
*** 把握する
Inboxに収集する
- [ ] Keep memo
- [ ] 紙の書類
- [ ] コミュニケーションツール
  - [ ] メール Inbox
  - [ ] メール ads
  - [ ] LINE
  - [ ] チャットツール
- [ ] カレンダーの予定を確認し、適切なアクションを登録する
*** 頭を空っぽにする
- [ ] 5分で新しいプロジェクト、連絡待ちの事柄、いつかやろうと思っていることなどを書き出し、頭を空にする
** 見極める
*** Inboxを空にする
- [ ] Inboxに入っている明らかになっていないものを仕分け、Inboxを空にする
  - すぐにできるものは実行
*** アクションリストを見直す
=C-c a a= でタスクを見る
- 完了したアクションはDONEにする
- 必要があればリマインダーをセットする
- WAITINGリストの更新
- 参考資料などを適切な場所に保存
*** 次に取るべき行動を考える
- [ ] アクションを推進するためのタスクを定義して追加する
  - プロジェクトリストや行動できていない項目について、次に取るべき行動は何かを考える
  - （たまに） M-x areas, projects で棚卸
- [ ] Resonance Calendar =C-c a r= で読みたい本や映画の予定を立てる
** 整理する
- [ ] プロジェクトリストの更新
  - 長期的なゴールやビジョンに沿ったアクションを見直す
  - プロジェクトリストを見返し、目標や結果の状態を一つ一つ評価する
  - 各項目について少なくとも1つの次の取るべき行動があることを確認する
- [ ] いつかやるリストの更新
  - [ ] =C-c n f= cancel で 中断中のプロジェクトを見返す
** 閉じる
- [ ] org-agenda タスクをアーカイブする
"
        :if-new (file+head ,file-path ,(format "#+title: %s\n" title))
        :immediate-finish t
        :unnarrowed t))))

(provide 'weekly-review)
;;; weekly-review.el ends here