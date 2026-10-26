;;; org-db-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; `org-db.el' の database 定義。
;;
;; 値はファイル先頭の file-level `:PROPERTIES:' drawer に置く
;; （level 0 ノードの properties）。見出しは作らない。
;; キーは Doom の leader から `my/db-open'（`SPC n D'）で開く。
;;; Code:

(require 'org-db)

(my/db-define "rez"
  :dir "resources/rez/"
  :columns
  '((title :width 40)
    ("Type" :type select :options ("Book" "Web" "Movie"))
    ("Start" :type date :default "today")
    ("Fin" :type date)
    ("Rating" :type select :options ("*" "**" "***" "****" "*****") :default "")
    ("Canceled" :type date :hidden t)
    ("Creator" :type text :hidden t)
    ("URL" :type text :hidden t)
    ("ReleaseDate" :type text :hidden t))
  :sort '("Start" . desc)
  :views
  '(("全部")
    ("進行中" :filter (and (empty "Fin") (empty "Canceled")))
    ("読了" :filter (not (empty "Fin"))))
  :capture-key "r"
  :capture-desc "rez (Resonance Calendar)"
  :body "* Why
# なぜ読もう・見ようと思ったのか

* Tasks
** TODO 読む・見る

* Key Ideas

* Review

* Quotes

* Notes
")

(my/db-define "exp"
  :dir "resources/exp/"
  :columns
  '((title :width 40)
    ("Type" :type select :options ("Exp") :default "Exp" :hidden t)
    ("Start" :type date)
    ("Fin" :type date)
    ("Assess" :type date)
    ("Qs" :type text)
    ("Status" :type text)
    ("Outcome" :type text))
  :sort '("Start" . desc)
  :views
  '(("全部"))
  :capture-key "e"
  :capture-desc "exp (Experiments)"
  :body "* Tasks

* 仮説

* 実験手順

* 観察とメモ

* 結果と考察

* 注意点

* 次の実験アイデア
")

;; org-roam-templates の後に読み込まれるので、同じキーの既存テンプレートを
;; 置き換える形で登録される。
(my/db-register-captures)

(provide 'org-db-config)
;;; org-db-config.el ends here
