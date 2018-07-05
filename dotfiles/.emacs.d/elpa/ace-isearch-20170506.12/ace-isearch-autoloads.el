;;; ace-isearch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ace-isearch" "../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch.el"
;;;;;;  "2529f3743b34eb81e2b06fb09930924e")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch.el

(autoload 'ace-isearch-jump-during-isearch "ace-isearch" "\
Jump to the one of the current isearch candidates.

\(fn)" t nil)

(autoload 'ace-isearch-mode "ace-isearch" "\
Minor-mode which combines isearch, ace-jump-mode, avy, helm-swoop and swiper seamlessly.

\(fn &optional ARG)" t nil)

(defvar global-ace-isearch-mode nil "\
Non-nil if Global Ace-Isearch mode is enabled.
See the `global-ace-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-ace-isearch-mode'.")

(custom-autoload 'global-ace-isearch-mode "ace-isearch" nil)

(autoload 'global-ace-isearch-mode "ace-isearch" "\
Toggle Ace-Isearch mode in all buffers.
With prefix ARG, enable Global Ace-Isearch mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Ace-Isearch mode is enabled in all buffers where
`ace-isearch--turn-on' would do it.
See `ace-isearch-mode' for more information on Ace-Isearch mode.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ace-isearch"
;;;;;;  "../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-isearch" '("ace-isearch-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/ace-isearch-20170506.12/ace-isearch.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-isearch-autoloads.el ends here
