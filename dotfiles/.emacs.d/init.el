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
(global-unset-key (kbd "C-u")) ;C-uは一旦無効化
(global-unset-key (kbd "C-t")) ; デフォルトのC-tを無効化
(global-unset-key (kbd "C-q")) ; デフォルトのC-q(特殊文字入力)を無効化
(global-unset-key (kbd "C-\\")) ;C-\(日本語入力)を無効化
(global-set-key (kbd "C-m") 'newline-and-indent) ; 改行してインデント
(global-set-key (kbd "C-x ?") 'help-command) ; ヘルプコマンド
(global-set-key (kbd "C-^") 'universal-argument) ; defaultのC-u
(global-set-key (kbd "C-M-d") 'kill-word) ; 単語ごとに削除
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; 折り返しをトグル
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース

;; ウィンドウ操作関連
(defun split-window-horizontally-n (num_wins)
  "任意の数だけ横に分割"
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

(defhydra hydra-window ()
  ("b" windmove-left)
  ("n" windmove-down)
  ("p" windmove-up)
  ("f" windmove-right)
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("C-<left>" shrink-window-horizontally)
  ("C-<right>" enlarge-window-horizontally)
  ("C-<up>" shrink-window)
  ("C-<down>" enlarge-window)
  ("s" window-swap-states)
  ("-" split-window-below)
  ("\\" split-window-right)
  ("0" delete-window)
  ("1" delete-other-windows)
  ("3" (lambda ()
	 "3分割"
	 (interactive)
	 (split-window-horizontally-n 3)))
  ("4" (lambda ()
	 "4分割"
	 (interactive)
	 (split-window-horizontally)
	 (split-window-vertically)
	 (setq i 0)
	 (while (< i 1)
	   (windmove-right)
	   (split-window-vertically)
	   (setq i (+ 1 i)))))
  ("6" (lambda ()
	 "6分割"
	 (interactive)
	 (split-window-horizontally-n 3)
	 (split-window-vertically)
	 (setq i 0)
	 (while (< i 2)
	   (windmove-right)
	   (split-window-vertically)
	   (setq i (+ 1 i)))))
  )
(global-set-key (kbd "C-t") 'hydra-window/body)


;;; @curxを使ったテキスト操作
(require 'crux)
(global-set-key (kbd "C-u") 'crux-smart-open-line-above) ;; 現在行を改行せずに上に空行を作ってその行に移動
(global-set-key (kbd "C-j") 'crux-smart-open-line) ;; 現在行を改行せずに下に空行を作ってその行に移動
(global-set-key (kbd "M-k") 'crux-kill-whole-line) ;; 現在行全体を削除して詰める
(global-set-key (kbd "M-h") 'crux-kill-line-backwards) ;; インデント位置からカーソル位置までを削除
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line) ;; C-a連打で行頭→行の最初のインデント位置への移動を繰り返す
(global-set-key (kbd "M-d") 'crux-duplicate-current-line-or-region) ;; 現在行or選択行を下に複製
(global-set-key (kbd "M-\\") 'crux-duplicate-and-comment-current-line-or-region) ;; 現在行or選択行を下に複製してコメントアウト



;;; ++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  オートセーブ、バックアップ
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/")) ; バックアップファイルはbackups/へ保存
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t))) ; オートセーブファイルもbackups/へ保存

;; 更新されたファイルを自動で読み直す
(global-auto-revert-mode t)
(setq create-lockfiles nil)



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

;; 行番号の表示/非表示を切り替え
(defun toggle-linum ()
  (interactive)
  (cond ((equal linum-format " ")
	 (setq linum-format "%3d "))
	(t
	 (setq linum-format " "))
	))
(global-set-key (kbd "C-c i") 'toggle-linum)

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

;; 入力補完
(electric-pair-mode t) ; 閉じ括弧自動挿入



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; elisp
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; emacs-lisp-mode
(defun elisp-mode-hooks ()
  "Lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks) ;Elipsの関数をモードラインに表示



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; HTML
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(defhydra hydra-web-mode-map (:color blue
				     :hint nil)
  "
^tide commands^
^-^--------------------------^-^------------------------^-^-------------------------
_eb_: element-beginning        _be_: block-end              _ab_: attribute-beginning
_ee_: element-end              _bn_: block-next             _ap_: attribute-previous
_en_: element-next             _bk_: block-kill             _at_: attribute-transpose
_ep_: element-previous         _bb_: block-beginning        _ae_: attribute-end
_ec_: element-child            _bc_: block-close            _ai_: attribute-insert
_el_: element-clone            _bs_: block-select           _as_: attribute-select
_eo_: element-close            _bp_: block-previous         _an_: attribute-next
_ew_: element-wrap                                          _ak_: attribute-kill
_es_: element-select           _tn_: tag-next               _cu_: comment-or-uncomment
_er_: element-rename           _tp_: tag-previous           _ci_: comment-indent-new-line
_ep_: element-parent           _tb_: tag-beginning          _ct_: toggle-comments
_ev_: element-vanish           _te_: tag-end
_ei_: element-insert           _tm_: tag-match              _dn_: dom-normalize
_ek_: element-kill             _ta_: tag-attributes-sort    _dq_: dom-quotes-replace
_et_: element-transpose        _ts_: tag-select             _da_: dom-apostrophes-replace
_em_: element-mute-blanks                                   _de_: dom-entities-replace
_cs_: element-content-select                                _dx_: dom-xpath
_ef_: element-children-fold-or-unfold                       _dt_: dom-traverse
"
  ("ee" web-mode-element-end)
  ("ek" web-mode-element-kill)
  ("en" web-mode-element-next)
  ("ec" web-mode-element-child)
  ("el" web-mode-element-clone)
  ("eo" web-mode-element-close)
  ("ew" web-mode-element-wrap)
  ("es" web-mode-element-select)
  ("er" web-mode-element-rename)
  ("ep" web-mode-element-parent)
  ("ev" web-mode-element-vanish)
  ("ei" web-mode-element-insert)
  ("ep" web-mode-element-previous)
  ("et" web-mode-element-transpose)
  ("eb" web-mode-element-beginning)
  ("em" web-mode-element-mute-blanks)
  ("cs" web-mode-element-content-select)
  ("ef" web-mode-element-children-fold-or-unfold)
  ("be" web-mode-block-end)
  ("bn" web-mode-block-next)
  ("bk" web-mode-block-kill)
  ("bb" web-mode-block-beginning)
  ("bc" web-mode-block-close)
  ("bs" web-mode-block-select)
  ("bp" web-mode-block-previous)
  ("tn" web-mode-tag-next)
  ("tp" web-mode-tag-previous)
  ("tb" web-mode-tag-beginning)
  ("te" web-mode-tag-end)
  ("tm" web-mode-tag-match)
  ("ta" web-mode-tag-attributes-sort)
  ("ts" web-mode-tag-select)
  ("ab" web-mode-attribute-beginning)
  ("ap" web-mode-attribute-previous)
  ("at" web-mode-attribute-transpose)
  ("ae" web-mode-attribute-end)
  ("ai" web-mode-attribute-insert)
  ("as" web-mode-attribute-select)
  ("an" web-mode-attribute-next)
  ("ak" web-mode-attribute-kill)
  ("cu" web-mode-comment-or-uncomment)
  ("ci" web-mode-comment-indent-new-line)
  ("ct" web-mode-toggle-comments)
  ("dn" web-mode-dom-normalize)
  ("dq" web-mode-dom-quotes-replace)
  ("da" web-mode-dom-apostrophes-replace)
  ("de" web-mode-dom-entities-replace)
  ("dx" web-mode-dom-xpath)
  ("dt" web-mode-dom-traverse)
  )




(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) ; HTMLのンデント幅
  (setq web-mode-css-indent-offset 2)  ; CSSのンデント幅
  (setq web-mode-code-indent-offset 2)  ; Ruby、PHP等のンデント幅
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq web-mode-enable-auto-closing 2) ; 閉じタグ自動補完
  (setq web-mode-enable-auto-pairing 2) ; 閉じタグ自動補完
  (add-hook 'after-save-hook 'web-mode-buffer-indent)
  (define-key web-mode-map (kbd "C-q") 'hydra-web-mode-map/body)
  (set (make-local-variable 'company-backends) '((company-web-html
  						  company-yasnippet
  						  company-dabbrev
  						  company-keywords
  						  company-capf
  						  company-files
  						  )
  						 (company-abbrev company-dabbrev)
  						 ))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; CSS
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; css-mode
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(defun setup-css-mode()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2)
  (prettier-js-mode)
  )
(add-hook 'css-mode-hook 'setup-css-mode)


(defun toggle-jsx-css-mode ()
  "rjsxモードとcssモードをトグル"
  (interactive)
  (with-current-buffer (buffer-name)  major-mode
		       (cond ((equal major-mode 'rjsx-mode)
			      (css-mode))
			     (t
			      (rjsx-mode))
			     )))
(global-set-key (kbd "C-c c s") 'toggle-jsx-css-mode)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; JavaScript
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defhydra hydra-js-mode-map (:color blue
				    :hint nil)
  "
^tide commands^
^-^----------------^-^---------------------^-^-----------------------------
_rs_: restart-server                 Restart tsserver. This would come in handy after you edit tsconfig.
_dp_: documentation-at-point         Show documentation for the symbol at point.
_rf_: references                     List all references to the symbol at point in a buffer.
_pe_: project-errors                 List all errors in the project. Errors can be navigated using n and p.
_rs_: rename-symbol                  Rename all occurrences of the symbol at point.
_rf_: rename-file                    Rename current file and all it's references in other files.
_fo_: format                         Format the current region or buffer.
_fi_: fix                            Apply code fix for the error at point. When invoked with a prefix arg,
_td_: add-tslint-disable-next-line   If the point is on one or more tslint
_re_: refactor                       Refactor code at point or current region.
_jt_: jsdoc-template                 Insert JSDoc comment template at point.
_vs_: verify-setup                   Show the version of tsserver.
_oi_: organize-imports               Organize imports in the file.
_jd_: jump-to-definition
_jb_: jump-back
"
  ("rs" tide-restart-server)
  ("dp" tide-documentation-at-point)
  ("rf" tide-references)
  ("pe" tide-project-errors)
  ("rs" tide-rename-symbol)
  ("rf" tide-rename-file)
  ("fo" tide-format)
  ("fi" tide-fix)
  ("td" tide-add-tslint-disable-next-line)
  ("re" tide-refactor)
  ("jt" tide-jsdoc-template)
  ("vs" tide-verify-setup)
  ("oi" tide-organize-imports)
  ("jd" tide-jump-to-definition)
  ("jb" tide-jump-back))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
            (setq js-indent-level 2) ;スペースは２つ、デフォルトは4
	    (setq js2-strict-missing-semi-warning nil) ;行末のセミコロンの警告はオフ
	    (setq js2-mode-show-parse-errors          nil)
            (setq js2-mode-show-strict-warnings       nil)
	    (js2-refactor-mode)
	    (prettier-js-mode)
            (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "location" "fetch")) ;指定した文字列の警告をオフ
	    (set (make-local-variable 'company-backends) '((company-tide
  							    company-yasnippet
  							    company-dabbrev
  							    company-keywords
  							    company-capf
  							    company-files
  							    )
  							   (company-abbrev company-dabbrev)
  							   ))))


;; rjsx-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))


;; tide-mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save) ;; formats the buffer before saving
  (define-key tide-mode-map (kbd "C-q") 'hydra-js-mode-map/body)
  (company-mode +1))
(setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setup-tide-mode)
;;   (prettier-js-mode)
;;   (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
;;   (setq web-mode-code-indent-offset 2) ;スペースは２つ、デフォルトは4
;;   (set (make-local-variable 'company-backends) '((company-tide
;;   						  company-dabbrev
;;   						  company-keywords
;;   						  company-capf
;;   						  company-files
;;   						  )
;;   						 (company-abbrev company-dabbrev)
;;   						 ))
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)


;; @Prettier-js
(require 'prettier-js)
(setq prettier-js-args '(
			 "--no-semi" "false"
			 "--jsx-bracket-same-line" "true"
			 "--trailing-comma" "es5"
			 "--arrow-parens" "always"
			 ))

;; @js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c <RET>")


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Golang
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; go-mode
(require 'go-mode)
(require 'go-guru)
(defun setup-go-mode ()
  "Hooks for Go mode."
  (setq tab-width 4)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (go-guru-hl-identifier-mode)
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
  (flycheck-mode)
  )


(defhydra hydra-go-mode-map (:color blue
				    :hint nil)
  "
^guru^             ^goto^                  ^other^
^-^----------------^-^---------------------^-^-----------------------------
_1_: describe      _a_: arguments          _d_: godoc-at-point
_2_: freevars      _s_: docstring          _o_: godoc
_3_: implements    _f_: function           _i_: go-import-add
_4_: peers         _n_: function-name      _r_: godoctor-rename
_5_: referrers     _v_: return-values      _e_: godoctor-extract
_6_: definition    _]_: method-receiver    _t_: godoctor-toggle
_7_: pointsto      ^ ^                     _@_: godoctor-godoc
_8_: callstack     ^ ^                     _:_: godef-describe
_9_: whicherrs     ^ ^                     _j_: godef-jump
_0_: callers       ^ ^                     _w_: godef-jump-other-window
_-_: callees       ^ ^                     ^ ^
"
  ("d" godoc-at-point)
  ("o" godoc)
  ("i" go-import-add)
  ("r" godoctor-rename)
  ("e" godoctor-extract)
  ("t" godoctor-toggle)
  ("@" godoctor-godoc)
  (":" godef-describe)
  ("j" godef-jump)
  ("w" godef-jump-other-window)

  ("a" go-goto-arguments)
  ("s" go-goto-docstring)
  ("f" go-goto-function)
  ("n" go-goto-function-name)
  ("v" go-goto-return-values)
  ("]" go-goto-method-receiver)

  ("1" go-guru-describe)
  ("2" go-guru-freevars)
  ("3" go-guru-implements)
  ("4" go-guru-peers)  ; c for channel
  ("5" go-guru-referrers)
  ("6" go-guru-definition) ; j for jump
  ("7" go-guru-pointsto)
  ("8" go-guru-callstack) ; s for stack
  ("9" go-guru-whicherrs) ; e for error
  ("0" go-guru-callers)
  ("-" go-guru-callees))

(defun setup-go-mode-map ()
  "Hooks for keybindings of go-mode."
  (define-key go-mode-map (kbd "C-c C-o") nil) ; go-guruのプリフィクス
  (define-key go-mode-map (kbd "C-c C-f") nil) ; go-modeのプリフィクス
  (define-key go-mode-map (kbd "C-c C-j") nil) ; godef-jump
  (define-key go-mode-map (kbd "C-x 4 C-c C-j") nil) ; godef-jump-other-window
  (define-key go-mode-map (kbd "C-c C-d") nil) ;godef-describe
  (define-key go-mode-map (kbd "C-c C-a") nil) ; go-import-add
  (define-key go-mode-map (kbd "C-q") 'hydra-go-mode-map/body)
  )

(add-hook 'go-mode-hook 'setup-go-mode)
(add-hook 'go-mode-hook 'setup-go-mode-map)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @helm
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'helm-config)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; helmでクリップボード履歴を表示
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'helm-for-files) ; helmでUIでカレントバッファとか見る
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c s") 'helm-swoop)



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
    (company-web dockerfile-mode yaml-mode go-guru hydra godoctor smartparens lispxmp open-junk-file go-eldoc company-go go-mode projectile yasnippet-snippets web-mode undo-tree tide rjsx-mode react-snippets rainbow-delimiters quickrun prettier-js json-mode js2-refactor helm-swoop helm-projectile helm-gtags expand-region crux company-tern company-statistics color-theme-solarized avy atom-one-dark-theme ace-isearch))))


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
(defhydra multiple-cursors-hydra (:hint nil)
  "
  ^Up^           ^Down^       ^Other^
----------------------------------------------
  _p_: Next      _n_: Next    _l_: Edit lines
  _P_: Skip      _N_: Skip    _a_: Mark all
_M-p_: Unmark  _M-n_: Unmark  _r_: Mark by regexp
^ ^             ^ ^           _q_: Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))
(global-set-key (kbd "C-\\") 'multiple-cursors-hydra/body)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @avy
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "M-g g") 'avy-goto-line)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @ace-isearch
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-ace-isearch-mode +1)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)) ;; 使用履歴＆ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case nil) ; 大文字、小文字を区別しない Emacs自体の設定
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
					;(add-hook 'after-init-hook #'global-flycheck-mode)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @projectile
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (when (require 'projectile nil t)
;;   (projectile-mode))                           ;;自動的にプロジェクト管理を開始

;; (when (require 'helm-projectile nil t)        ;; Helmを使って利用する
;;   (setq projectile-completion-system 'helm))
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; react-snippets.el
(require 'react-snippets)


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
(push '("godoc" :regexp t :height 0.4 :position top) popwin:special-display-config)
(push '("*quickrun*" :height 0.3) popwin:special-display-config)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @quickrun
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "C-c q b") 'quickrun)
(global-set-key (kbd "C-c q r") 'quickrun-region)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @open-junk-file
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @lispxmp
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @smartparens
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (require 'smartparens-config)
;; (smartparens-global-mode t)










;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !! DO NOT TOUCH !!

;;; init.el ends here
