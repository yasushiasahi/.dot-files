;;; packgage --- Summary
;;; Commentary:

;;; Code:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; EPLAパッケージの有効化
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'package) ; package.elを有効化
;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; インストール済みのElispを読み込む
;; 最新のpackageリストを読み込む
(when (not package-archive-contents)
  (package-refresh-contents))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 一般設定
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; yes/noはすべてy/nで答える
(defalias 'yes-or-no-p 'y-or-n-p)
;; 一行ずつスクロール
(setq scroll-conservatively 1)
;; 保存前にバッファ残体の行末の空行を削除する
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; キーバインド（一般）
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "C-m") 'newline-and-indent) ; 改行してインデント
(global-set-key (kbd "C-x ?") 'help-command) ; ヘルプコマンド
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース
(define-key key-translation-map (kbd "C-l") (kbd "<ESC>")) ; C-lでesc
(global-set-key (kbd "C-c r")  'recenter-top-bottom) ; 元のC-l
(global-unset-key (kbd "C-t")) ; デフォルトのC-tを無効化
(global-set-key (kbd "C-t C-b")  'windmove-left) ; 左のペインに移動
(global-set-key (kbd "C-t C-n")  'windmove-down) ; 下のペインに移動
(global-set-key (kbd "C-t C-p")    'windmove-up) ; 上のペインに移動
(global-set-key (kbd "C-t C-f") 'windmove-right) ; 右のペインに移動
(global-unset-key (kbd "C-q")) ; デフォルトのC-q(特殊文字入力)を無効化
(global-set-key (kbd "C-q") 'kill-ring-save) ; コピー
(global-unset-key (kbd "C-x 2"))
(global-set-key (kbd "C-x -") 'split-window-below) ; ウィンドウを縦分割
(global-unset-key (kbd "C-x 3"))
(global-set-key (kbd "C-x \\") 'split-window-right) ; ウィンドウを横分割
(global-set-key (kbd "C-M-d") 'kill-word) ; 単語ごとに削除
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; 折り返しをトグル
(global-set-key (kbd "C-c -") 'recenter-top-bottom) ; 折り返しをトグル
(global-set-key (kbd "C-c c s") 'css-mode) ; css-modo
(global-set-key (kbd "C-c j s") 'rjsx-mode) ; rjsx-modo



;; ウィンドウを縦3分割
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))
(global-set-key (kbd "C-x 3") (lambda ()
			  (interactive)
			  (split-window-horizontally-n 3)))

;; ウィンドウを4分割
(defun split4()
      (interactive)
      (split-window-horizontally)
      (split-window-vertically)
      (setq i 0)
      (while (< i 1)
      (windmove-right)
      (split-window-vertically)
      (setq i (+ 1 i))))
(global-set-key (kbd "C-x 4") 'split4)

;; ウィンドウを6分割
(defun split6()
      (interactive)
      (split-window-horizontally-n 3)
      (split-window-vertically)
      (setq i 0)
      (while (< i 2)
      (windmove-right)
      (split-window-vertically)
      (setq i (+ 1 i))))
(global-set-key (kbd "C-x 6") 'split6)

;; 行番号を表示
(defun set-linum ()
  (interactive)
  (linum-mode t)
  (setq linum-format "%3d "))
(global-set-key (kbd "C-c s l") 'set-linum)

;; 行番号を非表示
(defun unset-linum ()
  (interactive)
  (linum-mode t)
  (setq linum-format " "))
(global-set-key (kbd "C-c u l") 'unset-linum)



;;; @curxを使ったテキスト操作
(require 'crux)
(global-unset-key (kbd "C-u")) ; デフォルトのC-u(universal-argument)を無効化
(global-set-key (kbd "C-u") 'crux-smart-open-line-above) ;; 現在行を改行せずに上に空行を作ってその行に移動
(global-set-key (kbd "C-j") 'crux-smart-open-line) ;; 現在行を改行せずに下に空行を作ってその行に移動
(global-set-key (kbd "M-k") 'crux-kill-whole-line) ;; 現在行全体を削除して詰める
(global-set-key (kbd "M-h") 'crux-kill-line-backwards) ;; インデント位置からカーソル位置までを削除
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line) ;; C-a連打で行頭→行の最初のインデント位置への移動を繰り返す
(global-unset-key (kbd "C-\\")) ;C-\(日本語入力)を無効化
(global-set-key (kbd "C-\\") 'crux-indent-defun) ;; 選択不用で付近をいい感じにインデント
(global-set-key (kbd "M-d") 'crux-duplicate-current-line-or-region) ;; 現在行or選択行を下に複製
(global-set-key (kbd "M-\\") 'crux-duplicate-and-comment-current-line-or-region) ;; 現在行or選択行を下に複製してコメントアウト




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  オートセーブ、バックアップ
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/")) ; バックアップファイルはbackups/へ保存
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t))) ; オートセーブファイルもbackups/へ保存

;; 更新されたファイルを自動で読み直す
(global-auto-revert-mode t)
(setq create-lockfiles nil)





;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; メジャーモード
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; emacs-lisp-mode
(defun elisp-mode-hooks ()
  "Lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks) ;Elipsの関数をモードラインに表示

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) ; HTMLのンデント幅
  (setq web-mode-css-indent-offset 2)  ; CSSのンデント幅
  (setq web-mode-code-indent-offset 2)  ; Ruby、PHP等のンデント幅
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq web-mode-enable-auto-closing 2) ; 閉じタグ自動補完
  (setq web-mode-enable-auto-pairing 2) ; 閉じタグ自動補完
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; ;; rjsx-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
            (setq js-indent-level 2) ;スペースは２つ、デフォルトは4
            (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON")) ;指定した文字列の警告をオフ
            (setq js2-strict-missing-semi-warning nil) ;行末のセミコロンの警告はオフ
	    (setq company-backends '((
				      company-files
				      company-keywords
				      ompany-capf
				      company-tern
				      company-dabbrev-code
				      company-yasnippet
				      ) (company-abbrev company-dabbrev))))) ;加えたいcompanyの設定を書く


;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))
;; (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
;; (add-hook 'before-save-hook 'tide-format-before-save) ;; formats the buffer before saving



;; 入力補完
(electric-pair-mode t) ; 閉じ括弧自動挿入





;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 見た目
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 文字コード
(set-language-environment "Japanese") ; 日本語推奨環境
(prefer-coding-system 'utf-8) ; utf-8が最優先

;; モードライン
(column-number-mode t) ; カラム番号を表示
(size-indication-mode t) ; ファイスサイズを表示
(set-face-foreground 'mode-line "cyan")
(set-face-foreground 'mode-line-inactive "green")

;; 左端に行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format " ")
(set-face-background 'linum "brightblack")
(set-face-foreground 'linum "brightgreen")

;; 対応する括弧の強調表示
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
(setq show-paren-style 'parenthesis) ; カッコのみをハイライト
(set-face-foreground 'show-paren-match "brightblack")
(set-face-background 'show-paren-match "green")

(menu-bar-mode 0) ; メニューバー非表示
(setq inhibit-startup-screen t) ; スタートアップメッセージを非表示
(global-hl-line-mode t) ; 現在行をハイライト


;; theme
(when (eq system-type 'gnu/linux)
  (add-to-list 'custom-theme-load-path "/home/asahi/.dot-files/.emacs.d/custom-themes/emacs-color-theme-solarized"))
(when (eq system-type 'darwin)
  (add-to-list 'custom-theme-load-path "~/.dot-files/.emacs.d/custom-themes/emacs-color-theme-solarized"))
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @helm
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'helm-config)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; helmでクリップボード履歴を表示
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'helm-for-files) ; helmでUIでカレントバッファとか見る
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'helm-M-x)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @multiple-cursors
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'multiple-cursors)
(global-set-key (kbd "M-7") 'mc/edit-lines)
(global-set-key (kbd "M-9") 'mc/mark-next-like-this)
(global-set-key (kbd "M-8") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-0") 'mc/mark-all-like-this)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @ace-isearch
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-ace-isearch-mode +1)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)) ;; 使用履歴＆ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t) ; 大文字、小文字を区別しない Emacs自体の設定
(setq company-dabbrev-downcase nil) ; lower caseで補完で保管されるのを防ぐ
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fでも候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; 参考
; 「emacsの補完用パッケージcompany-mode」 https://qiita.com/sune2/items/b73037f9e85962f5afb7
; https://github.com/company-mode/company-mode/issues/407



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; company-tern
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq company-tern-property-marker "")
(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
  (let ((depth (get-text-property 0 'depth candidate)))
    (if (eq depth nil) 0 depth)))
(add-hook 'rjsx-mode-hook 'tern-mode) ; 自分が使っているjs用メジャーモードに変える



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @js2-refactor
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'rjsx-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c <RET>")


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @undo-tree
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-undo-tree-mode)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @expand-region
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'expand-region)
(global-set-key (kbd "C-o") 'er/expand-region)
;; expand-region.el/js-mode-expansions.el をjsx用に改変(html-mode-expansions.elの設定をコピペしただけ)。オリジナルはetc/の中。


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @flycheck
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @flycheck
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(package-install 'projectile)
(when (require 'projectile nil t)
  (projectile-mode)                           ;;自動的にプロジェクト管理を開始
  (add-to-list
    'projectile-globally-ignored-directories
    "node_modules")                           ;; プロジェクト管理から除外するディレクトリを追加
  (setq projectile-enable-caching t))         ;; プロジェクト情報をキャッシュする

(when (require 'helm-projectile nil t)        ;; Helmを使って利用する
  (setq projectile-completion-system 'helm))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)














;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" default)))
 '(package-selected-packages
   (quote
    (company-statistics yasnippet-snippets yasnippet quickrun helm-projectile projectile js2-mode tide crux expand-region js2-refactor atom-one-dark-theme company-tern rjsx-mode undo-tree company ace-isearch avy helm-swoop multiple-cursors web-mode helm moe-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!

;;; init.el ends here
