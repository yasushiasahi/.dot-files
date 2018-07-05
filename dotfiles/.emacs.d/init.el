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
(global-set-key (kbd "C-^") 'universal-argument) ; defaultのC-u
(global-unset-key (kbd "C-u")) ;C-uは一旦無効化
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


;;; rjsx-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;;; js2-mode
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
            (setq js-indent-level 2) ;スペースは２つ、デフォルトは4
            (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "location" "fetch")) ;指定した文字列の警告をオフ
            (setq js2-strict-missing-semi-warning nil) ;行末のセミコロンの警告はオフ
	    (set (make-local-variable 'company-backends) '((company-tide
  							    company-yasnippet
  							    company-dabbrev
  							    company-keywords
  							    company-capf
  							    company-files
  							    )
  							   (company-abbrev company-dabbrev)
  							   ))))


;;; tide-mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
(add-hook 'before-save-hook 'tide-format-before-save) ;; formats the buffer before saving
(add-hook 'js2-mode-hook #'setup-tide-mode)



;; go-mode
(require 'go-mode)
(defun setup-go-mode ()
  "Hooks for Go mode."
  (define-key go-mode-map (kbd "C-c g a") 'godoc-at-point)
  (define-key go-mode-map (kbd "C-c g d") 'godoc)
  (define-key go-mode-map (kbd "C-c g i") 'go-import-add)
  (setq tab-width 4)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (go-eldoc-setup)
  (set (make-local-variable 'company-backends) '((company-go
  						  company-yasnippet
  						  company-dabbrev
  						  company-keywords
  						  company-capf
  						  company-files
  						  )
  						 (company-abbrev company-dabbrev)
  						 ))
  (setq company-go-show-annotation t)
  (setq company-begin-commands '(self-insert-command))
  )
(add-hook 'go-mode-hook 'setup-go-mode)

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

;; リージョンの色
(set-face-foreground 'region "white")
(set-face-background 'region "brightgreen")

;;; @solarized  theme
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
;;; @helm-tags
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Enable helm-gtags-mode
(add-hook 'rjsx-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-gtags-auto-update t)
 '(helm-gtags-fuzzy-match t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-gtags-prefix-key nil)
 '(helm-gtags-pulse-at-cursor t)
 '(package-selected-packages
   (quote
    (go-eldoc company-go go-mode projectile yasnippet-snippets web-mode undo-tree tide rjsx-mode react-snippets rainbow-delimiters quickrun prettier-js json-mode js2-refactor helm-swoop helm-projectile helm-gtags expand-region crux company-tern company-statistics color-theme-solarized avy atom-one-dark-theme ace-isearch))))

;; key bindings
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "C-x d") 'helm-gtags-find-files)
  (define-key helm-gtags-mode-map (kbd "C-c n") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-c m") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c ,") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c .") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c /") 'helm-gtags-show-stack))




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

;;; +++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company-statistics
(require 'company-statistics)
(company-statistics-mode)

;;; @company-go
(require 'company-go)


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
;;; @projectile
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(when (require 'projectile nil t)
  (projectile-mode))                           ;;自動的にプロジェクト管理を開始

(when (require 'helm-projectile nil t)        ;; Helmを使って利用する
  (setq projectile-completion-system 'helm))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; react-snippets.el
(require 'react-snippets)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @Prettier-js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'prettier-js)
(setq prettier-js-args '(
  "--no-semi" "false"
  "--single-quote" "true"
  "--jsx-bracket-same-line" "true"
  "--print-width" "100"
))
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @google-translate
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'google-translate)
(require 'google-translate-default-ui)
(defvar google-translate-english-chars "[:ascii:]"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @popwin
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'popwin)
(popwin-mode 1)
(push '("*Google Translate*" :height 0.4)  popwin:special-display-config)
(push '("godoc" :regexp t  :height 0.4) popwin:special-display-config)











;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!

;;; init.el ends here
