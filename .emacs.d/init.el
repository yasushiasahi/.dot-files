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
;;; キーバインド（一般）
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "C-m") 'newline-and-indent) ; 改行してインデント
(global-set-key (kbd "C-x ?") 'help-command) ; ヘルプコマンド
(global-set-key (kbd "C-M-¥") 'indent-region) ; 自動インデント
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines) ; 折り返しをトグル
(global-unset-key (kbd "C-t")) ; デフォルトのC-tを無効化
(global-set-key (kbd "C-t C-b")  'windmove-left) ; 左のペインに移動
(global-set-key (kbd "C-t C-n")  'windmove-down) ; 下のペインに移動
(global-set-key (kbd "C-t C-p")    'windmove-up) ; 上のペインに移動
(global-set-key (kbd "C-t C-f") 'windmove-right) ; 右のペインに移動
(global-set-key (kbd "C-c r") 'cua-set-rectangle-mark) ;　矩形選択モード
(global-unset-key (kbd "C-\\")) ;C-\(日本語入力)を無効化

;; 現在行を改行せずに下に空行を作ってその行に移動
(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "C-j") 'smart-open-line)

;; 現在行を改行せずに上に空行を作ってその行に移動
(defun smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-j") 'smart-open-line-above)

;; カーソルより右側を下の行に送る(カーソル位置は動かない)
(defun open-line-next-indent ()
  (interactive)
  (newline-and-indent)
  (previous-line nil)
  (move-end-of-line nil))
(global-set-key (kbd "C-o") 'open-line-next-indent)

;; ウィンドウを縦3分割
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))
(global-set-key (kbd "C-x #") (lambda ()
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
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks) ;Elipsの関数をモードラインに表示




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 見た目
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Moe-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/moe-theme-20170914.2111/")
(add-to-list 'load-path "~/.emacs.d/elpa/moe-theme-20170914.2111/")
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0)) 
(moe-theme-set-color 'orange) ; モードラインの色
(moe-dark) ; ダークテーマ

;; モードライン
(column-number-mode t) ; カラム番号を表示
(size-indication-mode t) ; ファイスサイズを表示

;; 左端に行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")
(set-face-background 'linum "#282828")
(set-face-foreground 'linum "#aaa")

; 現在行をハイライト
(global-hl-line-mode t)

;; 対応する括弧の強調表示
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
(setq show-paren-style 'expression) ; 括弧内も強調

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; 一行ずつスクロール
(setq scroll-conservatively 1)

(menu-bar-mode 0) ; メニューバー非表示




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @helm
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'helm-config)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; helmでクリップボード履歴を表示
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-x C-f") 'helm-for-files) ; helmでUIでカレントバッファとか見る




























;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (helm moe-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!
