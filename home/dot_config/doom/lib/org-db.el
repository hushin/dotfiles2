;;; org-db.el -*- lexical-binding: t; -*-
;;; Commentary:
;; org-roam のノートを「1 ファイル = 1 行」のデータベースとして扱う汎用部分。
;;
;; - `my/db-define' で database（ディレクトリ・列・ビュー・capture）を定義する
;; - 値は org-roam の DB（level 0 ノードの properties）から読む。ファイルは読まない
;; - 値の実体はファイル先頭の file-level `:PROPERTIES:' drawer
;; - 編集時だけファイルを開いて `org-entry-put'/`org-entry-delete' し、
;;   `org-roam-db-update-file' で DB を更新する
;;
;; 具体的な database 定義は `org-db-config.el' にある。
;;
;; ソートは vtable 標準のものを使わず自前で行う。日付列を日付順に並べ、
;; 空値を昇順・降順どちらでも末尾に置くため（`my/db-sort-key' /
;; `my/db-compare-values' は純関数で、テストから直接呼べる）。
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-roam)
(require 'vtable)

;;;; 定義

(defvar my/db--tables (make-hash-table :test #'equal)
  "`my/db-define' で登録した database 定義。key は NAME 文字列。")

(defvar my/db-mode-map nil
  "`my/db-mode' のキーマップ。")

(defvar my/db--vtable-keymap nil
  "vtable のテキストプロパティに使うキーマップ。
モードのキーマップと違い親を持たせない（vtable が親を設定する）。")

(cl-defstruct (my/db-column (:constructor my/db--make-column))
  "database の 1 列。"
  name type options width hidden default default-p)

(cl-defstruct (my/db-table (:constructor my/db--make-table))
  "database の定義。"
  name dir columns sort views capture-key capture-desc body)

(cl-defstruct (my/db-row (:constructor my/db--make-row))
  "org ファイル 1 つ分の行。"
  id file title props)

(defun my/db--column-from-spec (spec)
  "列定義 SPEC を `my/db-column' にする。
SPEC は `(title :width 40)' か `(\"Type\" :type select :options (...))' の形。"
  (let* ((name (car spec))
         (plist (cdr spec))
         (type (or (plist-get plist :type)
                   (if (eq name 'title) 'title 'text))))
    (my/db--make-column
     :name name
     :type type
     :options (plist-get plist :options)
     :width (plist-get plist :width)
     :hidden (and (plist-get plist :hidden) t)
     :default (plist-get plist :default)
     :default-p (and (plist-member plist :default) t))))

(defun my/db-define (name &rest plist)
  "NAME の database を定義する。
PLIST のキー:
  :dir          `org-roam-directory' からの相対ディレクトリ
  :columns      列のリスト。`(title :width N)' と
                `(\"Prop\" :type text|select|date|number :options (...) :width N
                  :hidden BOOL :default STR)' の形を取る
  :sort         既定ソート `(\"Prop\" . desc|asc)'
  :views        名前付きビュー `((\"名前\" :filter EXPR :sort SPEC) ...)'
  :capture-key  capture テンプレートのキー
  :capture-desc capture テンプレートの説明
  :body         capture テンプレートの本文"
  (let ((table (my/db--make-table
                :name name
                :dir (or (plist-get plist :dir) "")
                :columns (mapcar #'my/db--column-from-spec
                                 (plist-get plist :columns))
                :sort (plist-get plist :sort)
                :views (plist-get plist :views)
                :capture-key (plist-get plist :capture-key)
                :capture-desc (plist-get plist :capture-desc)
                :body (plist-get plist :body))))
    (puthash name table my/db--tables)
    table))

(defun my/db-get (name)
  "NAME の database 定義を返す。"
  (or (gethash name my/db--tables)
      (user-error "未定義の database: %s" name)))

(defun my/db-names ()
  "定義済み database の名前を昇順で返す。"
  (sort (hash-table-keys my/db--tables) #'string<))

(defun my/db--column-label (column)
  "COLUMN の表示名を返す。"
  (let ((name (my/db-column-name column)))
    (if (eq name 'title) "Title" name)))

(defun my/db--column-by-name (table name)
  "TABLE の列 NAME を返す。"
  (seq-find (lambda (column)
              (equal name (my/db--column-label column)))
            (my/db-table-columns table)))

(defun my/db--visible-columns (table)
  "TABLE の表に出す列を返す。"
  (seq-remove #'my/db-column-hidden (my/db-table-columns table)))

(defun my/db--editable-columns (table)
  "TABLE の編集できる列を返す。hidden 列も候補に含める。"
  (seq-remove (lambda (column) (eq (my/db-column-type column) 'title))
              (my/db-table-columns table)))

;;;; 日付・ソートキー（純関数）

(defconst my/db--date-regexp "\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)"
  "active/inactive timestamp から日付部分を取り出す正規表現。")

(defun my/db-date-sort-key (value)
  "VALUE（`<2021-06-19 Sat>' など）から \"YYYY-MM-DD\" を取り出す純関数。
日付として解釈できなければ nil を返す。"
  (when (and (stringp value)
             (string-match my/db--date-regexp value))
    (match-string 1 value)))

(defun my/db-format-date (value)
  "date 型の VALUE を \"YYYY-MM-DD\" で表示する純関数。"
  (or (my/db-date-sort-key value)
      (or value "")))

(defun my/db-sort-key (type value)
  "TYPE と生の VALUE からソートキーを作る純関数。
空値（nil・空白のみ・日付として解釈できない date）は nil を返す。"
  (let ((string (and value (string-trim (substring-no-properties value)))))
    (when (and string (not (string-empty-p string)))
      (pcase type
        ('date (my/db-date-sort-key string))
        ('number (when (string-match-p "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" string)
                   (string-to-number string)))
        (_ (downcase string))))))

(defun my/db--key-lessp (_type a b)
  "ソートキー A と B を比較する。"
  (if (and (numberp a) (numberp b))
      (< a b)
    (string< (format "%s" a) (format "%s" b))))

(defun my/db-compare-values (type a b direction)
  "生の値 A と B を TYPE に従って比較する純関数。
DIRECTION が `desc' なら大小を反転する。空値は DIRECTION に関わらず末尾。
戻り値は -1（A が前）/ 0（同値）/ 1（A が後）。"
  (let ((ka (my/db-sort-key type a))
        (kb (my/db-sort-key type b)))
    (cond
     ((and (null ka) (null kb)) 0)
     ((null ka) 1)
     ((null kb) -1)
     ((my/db--key-lessp type ka kb) (if (eq direction 'desc) 1 -1))
     ((my/db--key-lessp type kb ka) (if (eq direction 'desc) -1 1))
     (t 0))))

(defun my/db-sort-rows (rows column direction)
  "ROWS を COLUMN でソートした新しいリストを返す純関数。
ROWS 自体は変更しない。同値のときはタイトル昇順にする。"
  (sort (copy-sequence rows)
        (lambda (row1 row2)
          (let ((result (my/db-compare-values
                         (my/db-column-type column)
                         (my/db--row-raw row1 column)
                         (my/db--row-raw row2 column)
                         direction)))
            (if (zerop result)
                (string< (my/db-row-title row1) (my/db-row-title row2))
              (< result 0))))))

;;;; フィルタ（純関数）

(defun my/db--filter-get (values field)
  "VALUES（(\"列名\" . 値) の alist）から FIELD の値を返す。"
  (cdr (assoc-string field values t)))

(defun my/db-eval-filter (expr values)
  "フィルタ式 EXPR を VALUES で評価する純関数。
EXPR は `(and ...)' `(or ...)' `(not X)' `(= \"列\" \"値\")' `(empty \"列\")'。
VALUES は `(\"列名\" . 表示値)' の alist。"
  (pcase expr
    (`(and . ,rest)
     (cl-every (lambda (sub) (my/db-eval-filter sub values)) rest))
    (`(or . ,rest)
     (cl-some (lambda (sub) (my/db-eval-filter sub values)) rest))
    (`(not ,sub)
     (not (my/db-eval-filter sub values)))
    (`(= ,field ,value)
     (equal (my/db--filter-get values field) value))
    (`(empty ,field)
     (string-empty-p (or (my/db--filter-get values field) "")))
    (_ (error "不正なフィルタ式: %S" expr))))

;;;; org-roam DB からの読み取り

(defun my/db--row-raw (row column)
  "ROW の COLUMN の生の値（title はタイトル、他はプロパティ文字列）。"
  (if (eq (my/db-column-type column) 'title)
      (my/db-row-title row)
    (cdr (assoc-string (my/db--column-label column)
                       (my/db-row-props row) t))))

(defun my/db-cell-value (row column)
  "ROW の COLUMN の表示値を返す。"
  (let ((raw (my/db--row-raw row column)))
    (pcase (my/db-column-type column)
      ('title (or raw ""))
      ('date (my/db-format-date raw))
      (_ (or (and raw (substring-no-properties raw)) "")))))

(defun my/db--row-values (table row)
  "ROW の表示値 alist。key は列名（title 列は \"Title\"）。"
  (let (values)
    (dolist (column (my/db-table-columns table))
      (push (cons (my/db--column-label column)
                  (my/db-cell-value row column))
            values))
    (nreverse values)))

(defun my/db--normalize-properties (properties)
  "PROPERTIES のキーを大文字に揃え、値からテキストプロパティを落とす。"
  (mapcar (lambda (pair)
            (cons (upcase (car pair))
                  (let ((value (cdr pair)))
                    (and value (substring-no-properties value)))))
          properties))

(defun my/db--node-row (node)
  "org-roam の NODE から `my/db-row' を作る。
値は `org-roam-node-properties'（キーは大文字）から取る。"
  (my/db--make-row
   :id (org-roam-node-id node)
   :file (or (org-roam-node-file node) "")
   :title (or (org-roam-node-title node) "")
   :props (my/db--normalize-properties (org-roam-node-properties node))))

(defun my/db--query-rows (&optional id)
  "org-roam のノードから行を読む。ID があればそのノードだけを読む。"
  (if id
      (when-let ((node (org-roam-node-from-id id)))
        (list (my/db--node-row node)))
    (mapcar #'my/db--node-row
            (seq-filter (lambda (node)
                          (equal 0 (org-roam-node-level node)))
                        (org-roam-node-list)))))

(defun my/db--in-directory-p (file dir)
  "FILE が DIR（ディレクトリ）の中にあるか。大文字小文字は区別しない。"
  (let ((file (expand-file-name file))
        (dir (file-name-as-directory (expand-file-name dir))))
    (and (>= (length file) (length dir))
         (eq t (compare-strings dir 0 (length dir) file 0 (length dir) t)))))

(defun my/db--directory (table)
  "TABLE の :dir を `org-roam-directory' からの絶対パスにする。"
  (file-name-as-directory
   (expand-file-name (my/db-table-dir table) org-roam-directory)))

(defun my/db--read-rows (table)
  "TABLE の :dir 配下の level 0 ノードを DB から読み、タイトル順で返す。
ファイルは読まず、`org-roam-node-properties' の値だけを使う。"
  (let ((dir (my/db--directory table)))
    (sort (seq-filter
           (lambda (row)
             (let ((file (my/db-row-file row)))
               (and (not (string-empty-p file))
                    (my/db--in-directory-p file dir))))
           (my/db--query-rows))
          (lambda (row1 row2)
            (string< (my/db-row-title row1) (my/db-row-title row2))))))

(defun my/db--read-row (id)
  "ID のノードを DB から読み直して返す。"
  (car (my/db--query-rows id)))

;;;; プロパティの書き込み

(defun my/db--set-property (file property value)
  "FILE の先頭（level 0）エントリの PROPERTY を VALUE にする。
VALUE が空なら PROPERTY を削除する。書き込み後 org-roam の DB を更新する。"
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (string-empty-p (or value ""))
          (org-entry-delete (point-min) property)
        (org-entry-put (point-min) property value)))
    (let ((inhibit-message t))
      (save-buffer))
    (org-roam-db-update-file file)))

(defun my/db--timestamp (time)
  "TIME を active timestamp `<(YYYY-MM-DD 曜日)>' にする純関数。"
  (let ((system-time-locale "C"))
    (format-time-string "<%Y-%m-%d %a>" time)))

;;;; capture

(defun my/db--capture-value (column now)
  "COLUMN の capture 時の値を返す純関数。NOW は date 型の today の基準。"
  (cond
   ((my/db-column-default-p column)
    (let ((default (my/db-column-default column)))
      (if (eq (my/db-column-type column) 'date)
          (if (equal default "today")
              (my/db--timestamp now)
            (or default ""))
        (or default ""))))
   ((and (eq (my/db-column-type column) 'select)
         (my/db-column-options column))
    (format "%%^{%s|%s}"
            (my/db--column-label column)
            (mapconcat #'identity (my/db-column-options column) "|")))
   (t "")))

(defun my/db-capture-head (table &optional now)
  "TABLE の capture 用 HEAD を生成する純関数。
先頭の `:PROPERTIES:' drawer（全列）と `#+title: ${title}' を返す。
NOW は date 型 :default `today' の基準時刻（省略時は現在時刻）。"
  (let ((now (or now (current-time)))
        (lines nil))
    (dolist (column (my/db-table-columns table))
      (unless (eq (my/db-column-type column) 'title)
        (let ((value (my/db--capture-value column now)))
          (push (format ":%s:%s" (my/db--column-label column)
                        (if (string-empty-p value) "" (concat " " value)))
                lines))))
    (concat ":PROPERTIES:\n"
            (mapconcat #'identity (nreverse lines) "\n")
            "\n:END:\n#+title: ${title}")))

(defun my/db-capture-template (table)
  "TABLE の org-roam capture テンプレートを返す。"
  (let ((key (my/db-table-capture-key table)))
    (unless key
      (user-error "%s には capture テンプレートがない" (my/db-table-name table)))
    (list key
          (or (my/db-table-capture-desc table) (my/db-table-name table))
          'plain
          (or (my/db-table-body table) "")
          :target (list 'file+head
                        (concat (file-name-as-directory (my/db-table-dir table))
                                "%<%Y%m%d%H%M%S>-${slug}.org")
                        (my/db-capture-head table))
          :unnarrowed t)))

(defun my/db-register-capture (table)
  "TABLE の capture テンプレートを `org-roam-capture-templates' に登録する。
同じキーの既存エントリは置き換える。"
  (let* ((template (my/db-capture-template table))
         (key (car template)))
    (setq org-roam-capture-templates
          (cons template (assoc-delete-all key org-roam-capture-templates)))
    template))

(defun my/db-register-captures ()
  "定義済み database の capture テンプレートをすべて登録する。"
  (interactive)
  (dolist (name (my/db-names))
    (let ((table (my/db-get name)))
      (when (my/db-table-capture-key table)
        (my/db-register-capture table)))))

(defun my/db-capture (table)
  "TABLE の capture テンプレートで新しいノートを作る。"
  (let ((key (my/db-table-capture-key table))
        (buffer (current-buffer))
        (dir (my/db--directory table)))
    (unless key
      (user-error "%s には capture テンプレートがない" (my/db-table-name table)))
    (let ((title (string-trim (read-string "タイトル: "))))
      (when (string-empty-p title)
        (user-error "タイトルが空です"))
      ;; 対象ディレクトリがまだ無ければ作る（org-capture は作らない）
      (unless (file-directory-p dir)
        (make-directory dir t))
      (my/db-register-capture table)
      (unwind-protect
          (org-roam-capture- :keys key
                             :node (org-roam-node-create :title title)
                             :templates org-roam-capture-templates)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (my/db--refresh t)))))))

;;;; バッファ表示

(defvar-local my/db--table nil
  "このバッファが表示している database 定義。")

(defvar-local my/db--view nil
  "現在のビュー名。")

(defvar-local my/db--filters nil
  "対話的に追加したフィルタ式のリスト（古い順）。")

(defvar-local my/db--sort nil
  "現在のソート `(\"列名\" . asc|desc)'。")

(defvar-local my/db--rows nil
  "DB から読んだ全行。")

(defvar-local my/db--shown nil
  "フィルタ・ソート後の表示中の行。")

(defvar-local my/db--vtable nil
  "現在の vtable オブジェクト。")

(defun my/db--view-def ()
  "現在のビューの定義 `(名前 . plist)' を返す。"
  (when my/db--view
    (assoc my/db--view (my/db-table-views my/db--table))))

(defun my/db--view-sort (table view-name)
  "VIEW-NAME のソート。なければ TABLE の既定ソート。"
  (or (plist-get (cdr (assoc view-name (my/db-table-views table))) :sort)
      (my/db-table-sort table)))

(defun my/db--filter-expr ()
  "現在有効なフィルタ式を返す。"
  (let ((exprs (delq nil (cons (plist-get (cdr (my/db--view-def)) :filter)
                               my/db--filters))))
    (pcase exprs
      ('nil nil)
      (`(,one) one)
      (many (cons 'and many)))))

(defun my/db--filtered-rows ()
  "現在のフィルタを適用した行を返す。"
  (let ((expr (my/db--filter-expr)))
    (if (null expr)
        my/db--rows
      (seq-filter (lambda (row)
                    (my/db-eval-filter expr
                                       (my/db--row-values my/db--table row)))
                  my/db--rows))))

(defun my/db--sorted-rows (rows)
  "現在のソートを適用した行を返す。"
  (let* ((spec my/db--sort)
         (column (and spec (my/db--column-by-name my/db--table (car spec)))))
    (if (null column)
        rows
      (my/db-sort-rows rows column (or (cdr spec) 'asc)))))

(defun my/db--row-matches-p (row)
  "ROW が現在のフィルタに一致するか。"
  (let ((expr (my/db--filter-expr)))
    (or (null expr)
        (my/db-eval-filter expr (my/db--row-values my/db--table row)))))

(defun my/db--header-string (rows)
  "表の上に出すヘッダ文字列を作る。"
  (let* ((table my/db--table)
         (view (or my/db--view "—"))
         (filter (if (my/db--filter-expr)
                     (prin1-to-string (my/db--filter-expr))
                   "なし"))
         (sort (if my/db--sort
                   (format "%s %s" (car my/db--sort)
                           (if (eq (cdr my/db--sort) 'desc) "降順" "昇順"))
                 "なし")))
    (propertize
     (format "%s — ビュー: %s — %d 件\nソート: %s / フィルタ: %s"
             (my/db-table-name table) view (length rows) sort filter)
     'keymap my/db-mode-map)))

(defun my/db--vtable-column (column)
  "COLUMN を vtable の列定義にする。"
  (append
   (list :name (my/db--column-label column)
         :getter (lambda (row _table) (my/db-cell-value row column)))
   (when (my/db-column-width column)
     (list :width (my/db-column-width column)))
   (when (eq (my/db-column-type column) 'number)
     (list :align 'right))))

(defun my/db--render ()
  "バッファローカルの状態から表を描き直す。"
  (let* ((table my/db--table)
         (previous (and my/db--vtable (vtable-current-object)))
         (columns (my/db--visible-columns table))
         (rows (my/db--sorted-rows (my/db--filtered-rows)))
         (inhibit-read-only t))
    (setq my/db--shown rows)
    (erase-buffer)
    (insert (my/db--header-string rows) "\n\n")
    (setq my/db--vtable
          (make-vtable
           :columns (mapcar #'my/db--vtable-column columns)
           :objects rows
           :keymap my/db--vtable-keymap
           :use-header-line t))
    ;; vtable のヘッダクリックによるソートは使わない（`S' でソートする）
    (vtable-header-mode -1)
    (goto-char (point-min))
    (when (vtable-goto-table my/db--vtable)
      (let ((target (if (and previous (memq previous rows))
                        previous
                      (car rows))))
        (when target
          (vtable-goto-object target))))))

(defun my/db--refresh (&optional reload)
  "表を描き直す。RELOAD なら DB から読み直す。"
  (when reload
    (setq my/db--rows (my/db--read-rows my/db--table)))
  (my/db--render))

(defun my/db--refresh-row (row column)
  "ROW を DB から読み直し、必要なら 1 行だけ描き直す。
COLUMN が現在のソート列なら並び順が変わるので全体を描き直す。"
  (if-let ((new (my/db--read-row (my/db-row-id row))))
      (progn
        (setf (my/db-row-file row) (my/db-row-file new)
              (my/db-row-title row) (my/db-row-title new)
              (my/db-row-props row) (my/db-row-props new))
        (if (or (and my/db--sort
                     (equal (car my/db--sort) (my/db--column-label column)))
                (not (my/db--row-matches-p row)))
            (my/db--render)
          (condition-case nil
              (if my/db--vtable
                  (vtable-update-object my/db--vtable row row)
                (my/db--render))
            (error (my/db--render)))))
    (my/db--render)))

;;;; 対話コマンド

(defun my/db--current-row ()
  "point の行を返す。"
  (or (vtable-current-object)
      (user-error "行の上で実行してください")))

(defun my/db--read-column (prompt columns)
  "PROMPT で COLUMNS から 1 列選ぶ。"
  (let* ((names (mapcar #'my/db--column-label columns))
         (name (completing-read prompt names nil t)))
    (seq-find (lambda (column) (equal name (my/db--column-label column)))
              columns)))

(defun my/db--read-value (column current)
  "COLUMN の新しい値を読む。空文字なら値を削除する。"
  (let ((label (my/db--column-label column)))
    (pcase (my/db-column-type column)
      ('select
       (completing-read (format "%s (空で削除): " label)
                        (my/db-column-options column) nil nil current))
      ('date
       (let ((input (string-trim
                     (read-string (format "%s (YYYY-MM-DD 等, 空で削除): " label)
                                  current))))
         (if (string-empty-p input)
             ""
           (my/db--timestamp (org-read-date nil t input)))))
      ('number
       (let ((input (string-trim
                     (read-string (format "%s (数値, 空で削除): " label)
                                  current))))
         (cond
          ((string-empty-p input) "")
          ((string-match-p "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" input) input)
          (t (user-error "数値ではありません: %s" input)))))
      (_
       (read-string (format "%s (空で削除): " label) current)))))

(defun my/db-open-row (&optional row)
  "ROW（省略時は point の行）のファイルを開く。"
  (interactive)
  (let ((row (or row (my/db--current-row))))
    (find-file (my/db-row-file row))))

(defun my/db-edit-cell (&optional row)
  "ROW（省略時は point の行）の列を選んで値を編集する。"
  (interactive)
  (let* ((row (or row (my/db--current-row)))
         (table my/db--table)
         (index (vtable-current-column))
         (visible (my/db--visible-columns table))
         (column (and index (< index (length visible)) (nth index visible)))
         (column (if (and column
                          (memq column (my/db--editable-columns table)))
                     column
                   (my/db--read-column "編集する列: "
                                       (my/db--editable-columns table))))
         (value (my/db--read-value column (my/db-cell-value row column))))
    (my/db--set-property (my/db-row-file row)
                         (my/db--column-label column)
                         value)
    (my/db--refresh-row row column)
    (message "%s を %s にしました"
             (my/db--column-label column)
             (if (string-empty-p value) "(空)" value))))

(defun my/db-add-row ()
  "新しい行（ノート）を capture する。"
  (interactive)
  (my/db-capture my/db--table))

(defun my/db--read-filter-value (column)
  "COLUMN のフィルタ値を読む。`(空)' を選ぶと `:empty' を返す。"
  (pcase (my/db-column-type column)
    ('select
     (if (my/db-column-options column)
         (let ((choice (completing-read "値: "
                                        (append (my/db-column-options column)
                                                (list "(空)"))
                                        nil t)))
           (if (equal choice "(空)") :empty choice))
       (my/db--read-filter-text)))
    ('date
     (let ((choice (completing-read "値: "
                                    (append (my/db--distinct-values column)
                                            (list "(空)"))
                                    nil nil)))
       (if (equal choice "(空)") :empty choice)))
    (_ (my/db--read-filter-text))))

(defun my/db--read-filter-text ()
  "テキストでフィルタ値を読む。"
  (let ((value (read-string "値 (空で (空)): ")))
    (if (string-empty-p value) :empty value)))

(defun my/db--distinct-values (column)
  "COLUMN の空でない表示値を重複なく返す。"
  (let (values)
    (dolist (row my/db--rows)
      (let ((value (my/db-cell-value row column)))
        (unless (string-empty-p value)
          (push value values))))
    (sort (delete-dups values) #'string<)))

(defun my/db-add-filter ()
  "列と値を選んでフィルタを追加する。"
  (interactive)
  (let* ((column (my/db--read-column "フィルタする列: "
                                     (my/db-table-columns my/db--table)))
         (label (my/db--column-label column))
         (value (my/db--read-filter-value column))
         (expr (if (eq value :empty) `(empty ,label) `(= ,label ,value))))
    (setq my/db--filters (append my/db--filters (list expr)))
    (my/db--render)
    (message "フィルタ: %s" (prin1-to-string (my/db--filter-expr)))))

(defun my/db-clear-filters ()
  "対話的に追加したフィルタをすべて解除する。
ビュー固有のフィルタは残る（ビューを切り替えると変わる）。"
  (interactive)
  (setq my/db--filters nil)
  (my/db--render)
  (message "フィルタを解除しました"))

(defun my/db-switch-view ()
  "名前付きビューを切り替える。"
  (interactive)
  (let ((views (my/db-table-views my/db--table)))
    (unless views
      (user-error "ビューが定義されていません"))
    (let ((name (completing-read "ビュー: " (mapcar #'car views) nil t
                                 nil nil my/db--view)))
      (setq my/db--view name
            my/db--filters nil
            my/db--sort (my/db--view-sort my/db--table name))
      (my/db--render)
      (message "ビュー: %s（%d 件）" name (length my/db--shown)))))

(defun my/db-reload ()
  "org-roam の DB から読み直して表示する。"
  (interactive)
  (my/db--refresh t)
  (message "再読み込みしました（%d 件）" (length my/db--rows)))

(defun my/db-sort-by-current-column ()
  "point の列（なければ選択した列）でソートする。
同じ列で繰り返すと昇順と降順を切り替える。"
  (interactive)
  (let* ((index (vtable-current-column))
         (visible (my/db--visible-columns my/db--table))
         (column (if (and index (< index (length visible)))
                     (nth index visible)
                   (my/db--read-column "ソートする列: " visible)))
         (label (my/db--column-label column))
         (descending (and my/db--sort
                          (equal (car my/db--sort) label)
                          (not (eq (cdr my/db--sort) 'desc))))
         (direction (if descending 'desc 'asc)))
    (setq my/db--sort (cons label direction))
    (my/db--render)
    (message "%s で %s" label (if descending "降順" "昇順"))))

;;;; モードとキーバインド

(defun my/db--bind-keys (map)
  "database バッファのキーを MAP に設定する。"
  (keymap-set map "RET" #'my/db-open-row)
  (keymap-set map "e" #'my/db-edit-cell)
  (keymap-set map "+" #'my/db-add-row)
  (keymap-set map "/" #'my/db-add-filter)
  (keymap-set map "\\" #'my/db-clear-filters)
  (keymap-set map "v" #'my/db-switch-view)
  (keymap-set map "g" #'my/db-reload)
  (keymap-set map "S" #'my/db-sort-by-current-column)
  (keymap-set map "q" #'quit-window)
  map)

(setq my/db-mode-map (my/db--bind-keys (make-sparse-keymap)))
(setq my/db--vtable-keymap (my/db--bind-keys (make-sparse-keymap)))

(define-derived-mode my/db-mode special-mode "db"
  "org-roam のノートを database として表示するモード。"
  (setq-local truncate-lines t))

(defun my/db-open (&optional name)
  "database NAME を `*db: NAME*' バッファに表示する。"
  (interactive
   (list (completing-read "Database: " (my/db-names) nil t)))
  (let* ((name (or name (completing-read "Database: " (my/db-names) nil t)))
         (table (my/db-get name))
         (view (car (car (my/db-table-views table))))
         (buffer (get-buffer-create (format "*db: %s*" name))))
    (with-current-buffer buffer
      (my/db-mode)
      (setq my/db--table table
            my/db--view view
            my/db--filters nil
            my/db--sort (my/db--view-sort table view)
            my/db--rows (my/db--read-rows table)
            my/db--vtable nil)
      (my/db--render))
    (pop-to-buffer buffer)))

(provide 'org-db)
;;; org-db.el ends here
