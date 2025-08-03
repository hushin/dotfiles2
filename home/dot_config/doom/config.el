;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ここに個人設定を記述してください！このファイルを変更した後に 'doom
;; sync' を実行する必要はありません！


;; 一部の機能では、GPG設定、メールクライアント、ファイルテンプレート、
;; スニペットなどで、ユーザーを識別するためにこれを使用します。オプションです。
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doomでフォントを制御するための5つの（オプション）変数が用意されています：
;;
;; - `doom-font' -- 使用するメインフォント
;; - `doom-variable-pitch-font' -- 非等幅フォント（該当する場合）
;; - `doom-big-font' -- `doom-big-font-mode' で使用；プレゼンテーションや
;;   ストリーミングに使用
;; - `doom-symbol-font' -- シンボル用
;; - `doom-serif-font' -- `fixed-pitch-serif' フェイス用
;;
;; これらが受け入れる値のドキュメントと例については、'C-h v doom-font' を参照してください。
;; 例：
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; フォントが見つからない場合は、'M-x describe-font' で検索し、
;; `M-x eval-region' でelispコードを実行し、'M-x doom/reload-font' で
;; フォント設定を更新してください。それでもEmacsがフォントを見つけられない場合は、
;; 正しくインストールされていない可能性があります。フォントの問題はDoomの問題ではありません！

;; テーマを読み込む方法は2つあります。どちらもテーマがインストールされて
;; 利用可能であることを前提としています。`doom-theme' を設定するか、
;; `load-theme' 関数で手動でテーマを読み込むことができます。これがデフォルトです：
(setq doom-theme 'doom-one)

;; これは行番号のスタイルを決定します。`nil' に設定すると、行番号は
;; 無効になります。相対行番号の場合は、これを `relative' に設定してください。
(setq display-line-numbers-type t)

;; `org' を使用していて、orgファイルを以下のデフォルトの場所に置きたくない場合は、
;; `org-directory' を変更してください。orgが読み込まれる前に設定する必要があります！
(setq org-directory "~/org/")


;; パッケージを再設定する際は、設定を `after!' ブロックで囲むようにしてください。
;; そうしないと、Doomのデフォルト設定が上書きされる可能性があります。例：
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; このルールの例外：
;;
;;   - ファイル/ディレクトリ変数の設定（`org-directory' など）
;;   - パッケージが読み込まれる前に設定することが明示的に指示されている変数の設定
;;     （ドキュメントを参照するには 'C-h v VARIABLE' を使用）
;;   - doom変数の設定（'doom-' または '+' で始まる変数）
;;
;; Doomの設定に役立つ追加の関数/マクロをご紹介します。
;;
;; - `load!' このファイルに相対的な外部の*.elファイルを読み込む
;; - `use-package!' パッケージを設定する
;; - `after!' パッケージが読み込まれた後にコードを実行する
;; - `add-load-path!' このファイルに相対的なディレクトリを `load-path' に追加する
;;   Emacsは `require' や `use-package' でパッケージを読み込む際に `load-path' を検索します
;; - `map!' 新しいキーをバインドする
;;
;; これらの関数/マクロの情報を取得するには、ハイライトされたシンボルにカーソルを
;; 移動して 'K' を押してください（non-evilユーザーは 'C-c c k' を押す）。
;; 使用方法のデモを含むドキュメントが開きます。
;; または、`C-h o' を使用してシンボル（関数、変数、フェイスなど）を検索することもできます。
;;
;; 'gd'（または 'C-c c d'）を試して定義にジャンプし、実装方法を確認することもできます。
