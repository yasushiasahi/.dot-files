;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yasushi Asahi

;; Author: Naoya Yamashita <asahi1600@gmail.com>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start writing settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-edit
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(leaf leaf
  :config
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf basic-keybindings
  :init
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (leaf crux
    :ensure t)
  :bind (("C-u")
	 ("C-t")
	 ("C-q")
	 ("C-\\")
	 ("C-m" . newline-and-indent)
	 ("M-/" . help-command)
	 ("C-^" . universal-argument)
	 ("C-M-d" . kill-word)
	 ("C-c l" . toggle-truncate-lines)
	 ("<f5>" . leaf-convert-insert-template)
	 ("<f6>" . leaf-convert-region-replace)
         ("C-u" . crux-smart-open-line-above)
	 ("C-j" . crux-smart-open-line)
	 ("M-k" . crux-kill-whole-line)
	 ("M-h" . crux-kill-line-backwards)
	 ("C-a" . crux-move-beginning-of-line)
	 ("M-d" . crux-duplicate-current-line-or-region)
	 ("M-\\" . crux-duplicate-and-comment-current-line-or-region)))

(leaf basic-settings
  :custom ((truncate-lines . t)                  ; do not wrap end of sentence
           (inhibit-startup-screen . t)          ; do not show startup screeen
           (scroll-preserve-screen-position . t) ; keep cursor position when scrolling
           (scroll-conservatively . 1)           ;
           (initial-scratch-message . "")        ; show nothing in scratch when statrup
           (visible-bell . t)                    ; show bell altanative of beep sound
           (auto-save-timeout . 15)
           (auto-save-interval . 60)
           (version-control . t)
           (delete-old-versions . t)
           )
  :hook ((before-save-hook . delete-trailing-whitespace)) ; delete trailing whitespace when save
  :preface
  (defalias 'yes-or-no-p 'y-or-n-p)     ; reduce typing, yas -> y, no -> n
  (when (eq window-system 'ns)
    (tool-bar-mode 0)                   ; no tool bar
    (scroll-bar-mode 0))                ; no scroll bar


  (when (eq window-system 'nil)         ; syncronise cripboard emacs and osx
    (defun copy-from-osx nil
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text)
      (let ((process-connection-type nil))
	(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	  (process-send-string proc text)
	  (process-send-eof proc))))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

  (leaf autorevert
    :doc "reflesh buffers when files on disk change"
    :custom ((auto-revert-interval . 1))
    :global-minor-mode global-auto-revert-mode)

  (leaf delsel
    :doc "overwrite region when yank or type"
    :global-minor-mode delete-selection-mode)

  (leaf recentf
    :custom ((recentf-max-saved-items . 1000)      ; max save limit
             (recentf-exclude . '(".recentf"))     ; exclude file list
             (recentf-auto-save-timer . '(run-with-idle-timer 30 t 'recentf-save-list))) ; after 30 second when no 作業 save .recentf file
    :global-minor-mode recentf-mode)

  (leaf paren
    :custom ((show-paren-delay . 0)
             (show-paren-style . 'mixed))
    :global-minor-mode show-paren-mode)

  (leaf language
    :preface
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-keyboard-coding-system 'utf-8))

  (leaf files
    :custom `((auto-save-timeout . 15)
              (auto-save-interval . 60)
              (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
              (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                          (,tramp-file-name-regexp . nil)))
              (version-control . t)
              (delete-old-versions . t)))
  (leaf startup
    :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))
  )

(leaf solarized-theme
  :ensure t
  :custom ((x-underline-at-descent-line . t)
           (solarized-emphasize-indicators . nil))
  :config
  (load-theme 'solarized-dark-high-contrast t))

(leaf fira-code-mode
    :ensure t
    :preface
    (set-face-attribute 'default nil :height 130)
    :global-minor-mode global-fira-code-mode)

(leaf mini-modeline
  :ensure t
  :global-minor-mode t)

(leaf window-customize
  :preface
  (defun split-window-horizontally-n (num_wins)
    "任意の数だけ横に分割"
    (interactive "p")
    (let ((n 1))
      (while (< n num_wins)
        (split-window-horizontally)
        (setq n (1+ n))))
    (balance-windows))
  (defhydra hydra-window nil
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
     (lambda ()
       (split-window-horizontally-n 3)))
    ("4"
     (lambda ()
       (split-window-horizontally-n 4))))
  :defun (split-window-horizontally-n)
  :bind (("C-t" . hydra-window/body)))

(leaf multiple-cursors
  :ensure t
  :bind (("C-q" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("|" mc/vertical-align)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf expand-region
  :ensure t
  :bind (("C-o" . er/expand-region)))

(leaf smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode)

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf vterm
  :ensure t
  :custom ((vterm-buffer-name-string . "vt %s"))
  :bind ((:vterm-mode-map
          ("C-t" . hydra-window/body))))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode)

(leaf projectile
  :ensure t
  :custom ((projectile-completion-system . 'ivy))
  :bind (("C-c p" . projectile-command-map))
  :global-minor-mode t)

(leaf all-the-icons
  :ensure t)

(leaf ivy
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

(leaf ivy-yasnippet
  :ensure t
  :bind (("C-c y" . ivy-yasnippet)))

(leaf ivy-prescient
  :ensure t
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf swiper
  :ensure t
  :bind (("C-c s" . swiper-thing-at-point)))

(leaf counsel
  :ensure t
  :bind (("M-C-r" . counsel-recentf)
         ("C-c i" . counsel-imenu)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x b" . counsel-switch-buffer))
  :custom ((counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions)))
  :global-minor-mode t)

(leaf counsel-projectile
  :ensure t
  :custom ((counsel-projectile-sort-files . t)
           (counsel-projectile-sort-projects . t)
           (counsel-projectile-mode . t)))


(leaf prescient
  :ensure t
  :global-minor-mode prescient-persist-mode)

(leaf avy
  :ensure t)

(leaf amx
  :ensure t
  :custom ((amx-history-length . 35)
	         (amx-backend . 'ivy)))

(leaf ace-isearch
  :ensure t
  :global-minor-mode global-ace-isearch-mode)

(leaf ivy-rich
  :ensure t)

(leaf all-the-icons-ivy-rich
  :ensure t
  :config
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

(leaf flycheck
  :ensure t)

(leaf yasnippet
  :ensure t
  :defun (yas-reload-all)
  :config
  (yas-reload-all)
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  :ensure t)

(leaf company
  :ensure t
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
           (company-minimum-prefix-length . 1))
  :global-minor-mode global-company-mode)

(leaf company-prescient
  :disabled t
  :ensure t
  :global-minor-mode t)

(leaf lsp-mode
  :ensure t
  :init
  (yas-global-mode)
  :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
         ((rust-mode-hook typescript-mode-hook) . lsp))
  :custom ((gc-cons-threshold . 104857600)
           (read-process-output-max . 1048576)
	   (lsp-idle-delay . 0.5)
	   (lsp-response-timeout . 5)
	   (lsp-completion-provider . :capf)
	   (lsp-prefer-capf . t)
           (lsp-keymap-prefix . "C-c l")
           (lsp-enable-indentation . nil)
           ;; rust
           (lsp-rust-analyzer-cargo-watch-command . "clippy")
           (lsp-rust-analyzer-proc-macro-enable . t)
           (lsp-rust-analyzer-server-display-inlay-hints . t)))

(leaf lsp-tailwindcss
  :ensure t
  :require t
  :custom ((lsp-tailwindcss-add-on-mode . t)))

(leaf lsp-ui
  :ensure t)

(leaf add-node-modules-path
  :ensure t
  :hook ((typescript-mode) . add-node-modules-path))

(leaf prettier
  :ensure t
  :global-minor-mode global-prettier-mode)

(leaf tree-sitter
  :ensure t
  :custom ((global-tree-sitter-mode . t))
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :ensure t
  :require t
  :defvar tree-sitter-major-mode-language-alist
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(react-tsx-mode . tsx)))

(leaf rustic
  :ensure t
  :custom ((rustic-format-trigger . 'on-save)))

(leaf typescript-mode
  :ensure t
  :preface
  (define-derived-mode react-tsx-mode typescript-mode "React")
  :mode ("\\.ts\\'"
         ("\\.tsx\\'" . react-tsx-mode))
  :custom ((typescript-indent-level . 2)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end writing settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
