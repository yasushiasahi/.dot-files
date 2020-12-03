;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; ここにいっぱい設定を書く
(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-13"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf leaf-convert
  :setq-default ((truncate-lines . t)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf leaf-convert
  :setq ((create-lockfiles))
  :config
  (add-to-list 'backup-directory-alist
	             (cons "." "~/.emacs.d/backups/"))
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/")
					                                t)))
  (global-auto-revert-mode t))

(leaf leaf-convert
  :custom ((inhibit-startup-screen . t)))

(leaf recentf
  :custom
  `((recentf-max-saved-items . 128)
    (recentf-auto-cleanup    . 'never)
    (recentf-exclude         . '(".recentf"
                                 "^/tmp\\.*"
                                 "^/private\\.*"
                                 "^/var/folders\\.*"
                                 "/TAGS$"
                                 "\\.*草稿\\.*"
                                 "^#\\.*"
                                 "^/home/uwabami/.mozilla/\\.*"
                                 "^/home/uwabami/.emacs.d/tmp/\\.*"
                                 "^/home/uwabami/.dotfiles/Emacs/tmp/\\.*"
                                 "^/[^/:]+:"
                                 "bookmarks"
                                 "org-recent-headings.dat"
                                 ))))


(leaf leaf-convert
  :bind (("C-u")
	       ("C-t")
	       ("C-q")
	       ("C-\\")
	       ("C-m" . newline-and-indent)
	       ("C-x ?" . help-command)
	       ("C-^" . universal-argument)
	       ("C-M-d" . kill-word)
	       ("C-c l" . toggle-truncate-lines)
	       ("<f5>" . leaf-convert-insert-template)
	       ("<f6>" . leaf-convert-region-replace)))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(leaf leaf-convert
  :config
  (delete-selection-mode t))

(leaf leaf-convert
  :hook ((before-save-hook . delete-trailing-whitespace)))

(leaf leaf-convert
  :custom ((show-paren-delay . 0)
	         (show-paren-style . 'mixed))
  :config
  (show-paren-mode t))

(leaf leaf-convert
  :setq ((scroll-conservatively . 1000)
	       (scroll-step . 1)
	       (scroll-preserve-screen-position . t)))

(leaf leaf-convert
  :config
  (setq default-directory (concat
                           (getenv "HOME")
                           "/")))

(defun my-set-fontsize (height)
  (set-face-attribute 'default nil :height height))

;; 文字コード
(leaf leaf-convert
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-face-attribute 'default nil :family "Cascadia Code" :height 150)
  (set-fontset-font nil 'japanese-jisx0208
                    (font-spec :family "Noto Sans CJK JP"))
  ;; リガチャ設定 https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (when (window-system)
    (set-frame-font "Cascadia Code"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))





(leaf leaf-convert
  :setq ((inhibit-startup-screen . t)
	       (scroll-conservatively . 1)
	       (initial-scratch-message . ""))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (when (eq window-system 'ns)
    (tool-bar-mode 0)
    (scroll-bar-mode 0))
  (when (eq window-system 'nil)
    (defun copy-from-osx nil
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
	      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	        (process-send-string proc text)
	        (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))

(leaf crux
  :doc "A Collection of Ridiculously Useful eXtensions"
  :req "seq-1.11"
  :tag "convenience"
  :added "2020-08-13"
  :url "https://github.com/bbatsov/crux"
  :ensure t
  :bind (("C-u" . crux-smart-open-line-above)
	       ("C-j" . crux-smart-open-line)
	       ("M-k" . crux-kill-whole-line)
	       ("M-h" . crux-kill-line-backwards)
	       ("C-a" . crux-move-beginning-of-line)
	       ("M-d" . crux-duplicate-current-line-or-region)
	       ("M-\\" . crux-duplicate-and-comment-current-line-or-region)))

(leaf solarized-theme
  :doc "The Solarized color theme"
  :req "emacs-24.1" "dash-2.16"
  :tag "solarized" "themes" "convenience" "emacs>=24.1"
  :added "2020-08-13"
  :url "http://github.com/bbatsov/solarized-emacs"
  :emacs>= 24.1
  :ensure t
  :custom((x-underline-at-descent-line . t)
          (solarized-emphasize-indicators . nil))
  :config
  (load-theme 'solarized-dark-high-contrast t))

(leaf expand-region
  :doc "Increase selected region by semantic units."
  :added "2020-08-21"
  :ensure t
  :bind (("C-o" . er/expand-region)))


(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :added "2020-08-13"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :added "2020-08-12"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :global-minor-mode global-flycheck-mode)


(leaf use-package
  :doc "A configuration macro for simplifying your .emacs"
  :req "emacs-24.3" "bind-key-2.4"
  :tag "package" "config" "speed" "startup" "dotemacs" "emacs>=24.3"
  :added "2020-08-13"
  :url "https://github.com/jwiegley/use-package"
  :emacs>= 24.3
  :ensure t)

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2020-12-02"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :added "2020-08-12"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :global-minor-mode global-company-mode
  :bind ((company-active-map
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-f" . company-complete-selection)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-complete-common-or-cycle)))
  :custom ((company-idle-delay . 0)
           (company-echo-delay . 0)
           (company-minimum-prefix-length . 1)))

(leaf company-box
  :doc "Company front-end with icons"
  :req "emacs-26.0.91" "dash-2.13" "dash-functional-1.2.0" "company-0.9.6"
  :tag "convenience" "front-end" "completion" "company" "emacs>=26.0.91"
  :added "2020-08-12"
  :url "https://github.com/sebastiencs/company-box"
  :emacs>= 26.0
  :ensure t
  :after company all-the-icons
  :custom ((company-box-max-candidates . 50)
           (company-box-icons-alist . 'company-box-icons-all-the-icons))
  :hook (company-mode-hook)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-fileicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
          (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
          (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
          (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
          (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
          (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
          (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
          (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
          (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
          (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
          (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
          (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
          (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
          (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
          (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
          (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
          (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
          (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
          (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))

(leaf company-posframe
  :doc "Use a posframe as company candidate menu"
  :req "emacs-26.0" "company-0.9.0" "posframe-0.1.0"
  :tag "matching" "convenience" "abbrev" "emacs>=26.0"
  :url "https://github.com/tumashu/company-posframe"
  :added "2020-08-12"
  :emacs>= 26.0
  :ensure t
  :after company posframe
  :global-minor-mode t)

(leaf company-prescient
  :disabled t
  :doc "prescient.el + Company"
  :req "emacs-25.1" "prescient-4.0" "company-0.9.6"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :added "2020-08-12"
  :emacs>= 25.1
  :ensure t
  :after prescient company
  :global-minor-mode t)

(leaf company-quickhelp
  :disabled t
  :doc "Popup documentation for completion candidates"
  :req "emacs-24.3" "company-0.8.9" "pos-tip-0.4.6"
  :tag "quickhelp" "documentation" "popup" "company" "emacs>=24.3"
  :url "https://www.github.com/expez/company-quickhelp"
  :added "2020-08-12"
  :emacs>= 24.3
  :ensure t
  :after company pos-tip
  :custom ((company-quickhelp-delay . 0.8)
           (company-quickhelp-mode . t))
  :bind (company-active-map
         ("M-h" . company-quickhelp-manual-begin))
  :hook (company-mode-hook))

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3" "memoize-1.0.1"
  :tag "lisp" "convenient" "emacs>=24.3"
  :added "2020-08-12"
  :url "https://github.com/domtronn/all-the-icons.el"
  :emacs>= 24.3
  :ensure t
  :after memoize)

(leaf all-the-icons-dired
  :doc "Shows icons for each file in dired mode"
  :req "emacs-24.4" "all-the-icons-2.2.0"
  :tag "dired" "icons" "files" "emacs>=24.4"
  :added "2020-08-12"
  :url "https://github.com/jtbm37/all-the-icons-dired"
  :emacs>= 24.4
  :ensure t
  :after all-the-icons)

(leaf all-the-icons-ivy
  :doc "Shows icons while using ivy and counsel"
  :req "emacs-24.4" "all-the-icons-2.4.0" "ivy-0.8.0"
  :tag "faces" "emacs>=24.4"
  :added "2020-08-12"
  :emacs>= 24.4
  :ensure t
  :after all-the-icons ivy
  :custom ((all-the-icons-ivy-file-commands . '(counsel-find-file
					                                      counsel-file-jump
					                                      counsel-recentf
					                                      counsel-ibuffer
                                                counsel-switch-buffer
					                                      counsel-projectile
					                                      counsel-projectile-find-file
					                                      counsel-projectile-find-dir
					                                      ivy-switch-buffer))
           (all-the-icons-ivy-buffer-commands . '(counsel-find-file
					                                        counsel-file-jump
					                                        counsel-recentf
					                                        counsel-ibuffer
                                                  counsel-switch-buffer
					                                        counsel-projectile
					                                        counsel-projectile-find-file
					                                        counsel-projectile-find-dir
					                                        ivy-switch-buffer)))
  :config
  (all-the-icons-ivy-setup))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :added "2020-08-13"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :custom ((ivy-truncate-lines)
	         (ivy-wrap . t)
	         (enable-recursive-minibuffers . t)
	         (ivy-fixed-height-minibuffer . t)
	         (ivy-count-format . "(%d/%d) ")
           (ivy-height-alist . '((t
                                 lambda (_caller)
                                 (/ (frame-height) 3)))))
  :global-minor-mode t)

(leaf ivy-rich
  :doc "More friendly display transformer for ivy"
  :req "emacs-25.1" "ivy-0.13.0"
  :tag "ivy" "convenience" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/Yevgnen/ivy-rich"
  :emacs>= 25.1
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf all-the-icons-ivy-rich
  :doc "Better experience with icons for ivy"
  :req "emacs-25.1" "ivy-rich-0.1.0" "all-the-icons-2.2.0"
  :tag "ivy" "icons" "convenience" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/seagle0128/all-the-icons-ivy-rich"
  :emacs>= 25.1
  :ensure t
  :after ivy-rich all-the-icons
  :global-minor-mode t)


(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-5.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf swiper
  :doc "Isearch with an overview. Oh, man!"
  :req "emacs-24.5" "ivy-0.13.0"
  :tag "matching" "emacs>=24.5"
  :added "2020-08-13"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :bind (("C-c s" . swiper)))

(leaf counsel
  :doc "Various completion functions using Ivy"
  :req "emacs-24.5" "swiper-0.13.0"
  :tag "tools" "matching" "convenience" "emacs>=24.5"
  :added "2020-08-13"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :after swiper
  :bind (("M-C-r" . counsel-recentf)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x b" . counsel-switch-buffer))
  :custom ((counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions)))
  :global-minor-mode t)

(leaf counsel-projectile
  :doc "Ivy integration for Projectile"
  :req "counsel-0.13.0" "projectile-2.0.0"
  :tag "convenience" "project"
  :added "2020-08-13"
  :url "https://github.com/ericdanan/counsel-projectile"
  :ensure t
  :after counsel projectile
  :custom ((counsel-projectile-sort-files . t)
           (counsel-projectile-sort-projects . t)
           (counsel-projectile-mode . t)))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode prescient-persist-mode)

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :added "2020-08-13"
  :url "https://github.com/abo-abo/avy"
  :emacs>= 24.1
  :ensure t)

(leaf amx
  :doc "Alternative M-x with extra features."
  :req "emacs-24.4" "s-0"
  :tag "usability" "convenience" "emacs>=24.4"
  :added "2020-08-13"
  :url "http://github.com/DarwinAwardWinner/amx/"
  :emacs>= 24.4
  :ensure t
  :custom ((amx-history-length . 35)
	         (amx-backend . 'ivy)))

(leaf ace-isearch
  :doc "A seamless bridge between isearch, ace-jump-mode, avy, helm-swoop and swiper"
  :req "emacs-24"
  :tag "emacs>=24"
  :added "2020-08-13"
  :url "https://github.com/tam17aki/ace-isearch"
  :emacs>= 24
  :ensure t
  :global-minor-mode global-ace-isearch-mode)

(leaf smartparens
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."
  :req "dash-2.13.0" "cl-lib-0.3"
  :added "2020-08-13"
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode)

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :added "2020-08-13"
  :url "http://www.dr-qubit.org/emacs.php"
  :ensure t
  :global-minor-mode global-undo-tree-mode)

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :added "2020-08-13"
  :url "https://github.com/justbur/emacs-which-key"
  :emacs>= 24.4
  :ensure t
  :global-minor-mode t)

(leaf leaf-convert
  :bind (("C-t" . hydra-window/body))
  :config
  (defun split-window-horizontally-n (num_wins)
    "任意の数だけ横に分割"
    (interactive "p")
    (dotimes (i
	            (- num_wins 1))
      (split-window-horizontally))
    (balance-windows))

  (defhydra hydra-window nil
    ("t" neotree-toggle)
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
    ("3"
     (lambda nil
       "3分割"
       (interactive)
       (split-window-horizontally-n 3)))
    ("4"
     (lambda nil
       "4分割"
       (interactive)
       (split-window-horizontally-n 4)))
    ("6"
     (lambda nil
       "6分割"
       (interactive)
       (split-window-horizontally-n 3)
       (split-window-vertically)
       (setq i 0)
       (while (< i 2)
	       (windmove-right)
	       (split-window-vertically)
	       (setq i (+ 1 i)))))))

(leaf wgrep
  :doc "Writable grep buffer and apply the changes to files"
  :tag "extensions" "edit" "grep"
  :added "2020-11-19"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el"
  :ensure t)

(leaf leaf-convert
  :setq ((scroll-conservatively . 1)))

(leaf vterm
  :doc "Fully-featured terminal emulator"
  :req "emacs-25.1"
  :tag "terminals" "emacs>=25.1"
  :added "2020-11-27"
  :url "https://github.com/akermu/emacs-libvterm"
  :emacs>= 25.1
  :ensure t
  :custom ((vterm-buffer-name-string . "vt %s"))
  :bind ((:vterm-mode-map
          ("C-t" . hydra-window/body))))

(leaf goto-chg
  :doc "goto last change"
  :tag "matching" "convenience"
  :added "2020-11-27"
  :url "https://github.com/emacs-evil/goto-chg"
  :ensure t
  :bind (("M-[" . hydra-goto-chg/goto-last-change)
         ("M-[" . hydra-goto-chg/goto-last-change-reverse))
  :config
  (defhydra hydra-goto-chg ()
    ("[" goto-last-change)
    ("]" goto-last-change-reverse)))

(leaf neotree
  :doc "A tree plugin like NerdTree for Vim"
  :req "cl-lib-0.5"
  :added "2020-08-13"
  :url "https://github.com/jaypei/emacs-neotree"
  :ensure t
  :custom ((neo-theme . 'ascii)
           (neo-persist-show . t)
           (neo-smart-open . t)
           (neo-show-hidden-files . t)
           (neo-theme . 'icons)
           (neo-create-file-auto-open . t)))

(leaf open-junk-file
  :doc "Open a junk (memo) file to try-and-error"
  :tag "tools" "convenience"
  :added "2020-10-29"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el"
  :ensure t
  :bind (("C-c j" . open-junk-file))
  :custom ((open-junk-file-find-file-function . 'find-file))
  )

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :req "cl-lib-0.5"
  :added "2020-08-13"
  :ensure t
  :bind (("C-q" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))


(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :custom ((projectile-completion-system . 'ivy))
  :bind (("C-c p" . projectile-command-map))
  :global-minor-mode t)

(leaf with-editor
  :doc "Use the Emacsclient as $EDITOR"
  :req "emacs-24.4" "async-1.9"
  :tag "tools" "emacs>=24.4"
  :added "2020-11-27"
  :url "https://github.com/magit/with-editor"
  :emacs>= 24.4
  :ensure t)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-13"
  :emacs>= 25.1
  :ensure t
  :after with-editor
  :custom ((magit-completing-read-function . 'ivy-completing-read)))

(leaf add-node-modules-path
  :doc  "Add node_modules to your exec-path"
  :tag "eslint" "node_modules" "node" "javascript"
  :added "2020-08-13"
  :url "https://github.com/codesuki/add-node-modules-path"
  :ensure t
  :hook ((js-mode-hook css-mode-hook web-mode-hook scss-mode typescript-mode) . add-node-modules-path))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2020-08-13"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
         (web-mode-hook. lsp-deferred)
         (css-mode-hook. lsp-deferred)
         (scss-mode-hook. lsp-deferred)
         (typescript-mode-hook . lsp-deferred))
  :custom ((gc-cons-threshold . 100000000)
	         (lsp-idle-delay . 0.5)
	         (lsp-response-timeout . 5)
	         (lsp-completion-provider . :capf)
	         (lsp-prefer-capf . t)
           (lsp-keymap-prefix . "C-c l"))
  :config
  (setq read-process-output-max (* 1024 1024)))

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=26.1"
  :added "2020-08-13"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode markdown-mode
  :commands lsp-ui-mode)

(leaf lsp-ivy
  :doc "LSP ivy integration"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
  :tag "debug" "languages" "emacs>=25.1"
  :added "2020-08-13"
  :url "https://github.com/emacs-lsp/lsp-ivy"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode ivy
  :commands lsp-ivy-workspace-symbol)

(leaf lsp-treemacs
  :doc "LSP treemacs"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
  :tag "languages" "emacs>=26.1"
  :added "2020-08-13"
  :url "https://github.com/emacs-lsp/lsp-treemacs"
  :emacs>= 26.1
  :ensure t
  :after treemacs lsp-mode
  :commands lsp-treemacs-errors-list)

(leaf prettier-js
  :doc "Minor mode to format JS code on file save"
  :tag "js" "edit" "wp" "convenience"
  :added "2020-08-13"
  :url "https://github.com/prettier/prettier-emacs"
  :ensure t)

(leaf auto-shell-command
  :doc "Run the shell command asynchronously that you specified when you save the file."
  :req "deferred-20130312" "popwin-20130329"
  :tag "auto" "deferred" "async" "save" "shell"
  :added "2020-11-19"
  :ensure t
  :after deferred
  :config
  (ascmd:add '("/Users/zero.asahi/dev/src/github.com/karabiner-tokushimaru/tokushimaru_portal_system_source/resources/views/tokushimaru/**/.*\.blade.php"       "blade-formatter --w $FILE")))




(leaf scss-mode
  :doc "Major mode for editing SCSS files"
  :tag "mode" "css" "scss"
  :added "2020-08-13"
  :url "https://github.com/antonj/scss-mode"
  :ensure t
  :custom ((css-indent-offset . 2)))

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2020-08-13"
  :url "http://web-mode.org"
  :emacs>= 23.1
  :ensure t
  :mode ("\\.html\\'" "\\.php\\'" "\\.tsx\\'" "\\.jsx\\'" "\\.vue\\'" "\\.xml\\'")
  :custom ((web-mode-attr-indent-offset)
	         (web-mode-markup-indent-offset . 2)
	         (web-mode-css-indent-offset . 2)
	         (web-mode-code-indent-offset . 2)
	         (web-mode-sql-indent-offset . 2)
	         (indent-tabs-mode)
	         (tab-width . 2)
	         (web-mode-script-padding . 0)
	         (web-mode-style-padding . 0)
	         (web-mode-block-padding . 0)
	         (web-mode-enable-current-element-highlight . t)
	         (web-mode-enable-current-column-highlight . t)
	         (web-mode-enable-auto-closing . t)
	         (web-mode-enable-auto-expanding . t)
	         (web-mode-comment-style . 2))
  ;; :config
  ;; (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
  ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
  )

;; (leaf jsx-mode
;;   :mode ("\\.jsx\\'" "\\.tsx\\'" "\\.ts\\'" "\\.js\\'"))

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :added "2020-08-23"
  :url "http://github.com/ananthakumaran/typescript.el"
  :emacs>= 24.3
  :ensure t
  :custom ((typescript-indent-level . 2)))

(leaf go-mode
  :doc "Major mode for the Go programming language"
  :tag "go" "languages"
  :added "2020-09-16"
  :url "https://github.com/dominikh/go-mode.el"
  :ensure t
  :custom ((gofmt-command . "goimports"))
  :hook ((go-mode-hook . (lambda ()
                           (setq tab-width 4)
                           (lsp-deferred)))
         (before-save-hook . gofmt-before-save)))



(provide 'init)


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
