(provide 'init)
;; Make sure we load this file, instead of the precompiled file, if it is newer
(setq load-prefer-newer t)

(setq use-package-compute-statistics t)

(setq gc-cons-threshold (* 50 1024 1024))

;; Silence compiler warnings, as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Setup the package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
;; (unless package-archive-contents
;;    (package-refresh-contents))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
             :ensure t)



;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)


(if (not (fboundp 'x-hide-tip))
    (defun x-hide-tip ()
      (interactive)))

(require 'cl-lib)

(use-package emacs
  :bind (("RET" . newline-and-indent))
  :config
  (setq tab-always-indent t)
  (setq resize-mini-windows t)
  (setq max-mini-window-height 0.2)
  ;; Setup the display-buffer-alist
  (setq display-buffer-alist nil))
  ;; (add-to-list 'display-buffer-alist
  ;;               '("magit:.*?"
  ;;                 (display-buffer-reuse-window display-buffer-in-side-window)
  ;;                 (side . right)
  ;;                 (dedicated . nil)
  ;;                 (window-width . 80))))




(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))




(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-lah --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))


;; Try to use UTF-8 for everything
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ;; Catch-all
;; (global-hl-line-mode 1)
(setq scroll-conservatively -1)

(setq enable-local-variables nil)



(c-add-style "my-c-style" '((c-tab-always-indent . t)
                            (c-basic-offset . 4)
                            (c-offsets-alist (access-label . 1)
                                             (label . +))))
;; Setting this as the default style:
(setq c-default-style "my-c-style")
(setq c-ts-default-style "my-c-style")

;; Make it so underscores and dashes are considered part of a word
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

(require 'pl-greek)


(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)



(global-display-line-numbers-mode -1)
(electric-indent-mode -1)



;; The most important thing we want here is evil mode, cause I can't use emacs without it :)
(setq evil-want-keybinding nil)


(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (define-key evil-normal-state-map (kbd ".") #'er/expand-region)
  (define-key evil-visual-state-map (kbd ".") #'er/expand-region)
  (define-key evil-visual-state-map (kbd ",") #'er/contract-region)

  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "C-n") 'neotree)
  (define-key evil-normal-state-map (kbd "SPC")
              (lambda ()
                (interactive)
                (evil-execute-macro 1 "viw")))
  (evil-set-leader 'motion (kbd "\\"))
  :init
  (evil-mode 1))

(use-package evil-collection
  :init
  (evil-collection-init))


(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :ensure t
    :init
    (evil-terminal-cursor-changer-activate)))

(setq evil-insert-state-cursor 'bar)

(evil-ex-define-cmd "W" "w")









(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev nil) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  (lambda-line-vc-symbol " ")
  :config
  ;; activate lambda-line
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))






(cl-defun setup-repl (map &key run-buffer send-to-repl)
  (define-key map (kbd "C-c e") run-buffer)
  (define-key map (kbd "C-c C-r") send-to-repl))

(setup-repl emacs-lisp-mode-map
            :run-buffer #'eval-buffer)



(use-package racket-mode
  :ensure t
  :config
   (add-hook 'racket-mode-hook
       (lambda ()
         (racket-xp-mode)
         (set-input-method 'pl-greek)
         (setup-repl racket-mode-map
                     :run-buffer #'racket-run
                     :send-to-repl #'racket-send-last-sexp))))

;; ;; Use geiser to make Guile development nicer
;; (use-package geiser-guile
;;   :ensure t
;;   :config
;;   (add-hook 'geiser-mode-hook
;;             (lambda ()
;;               (geiser)
;;               (setup-repl geiser-mode-map :run-buffer #'geiser-eval-buffer
;;                                           :send-to-repl #'geiser-eval-last-sexp))))



(use-package expand-region
  :ensure t)


(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-setup-minibuffer)
  :config
  (which-key-mode))

(use-package magit
  :ensure t)

;; ====--------------------------------------------------====

;; I'm told that these few packages make the minibuffer much nicer to use.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-c f" . #'consult-ripgrep)))


(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-," . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package neotree
  :ensure t)







(when (display-graphic-p)
  (set-frame-font "JetBrains Mono Bold 12" nil t))




(defun ncw/setup-lsp-mode ()
  (message "ncw/setup-lsp-mode called")
  (company-mode 1)
  ;; (lsp-which-key-integration)
  (lsp-diagnostics-mode 1)
  (lsp-completion-mode 1))


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-ts-mode . lsp)
         (c-mode . lsp)
         (c++-ts-mode . lsp)
         ;; (c++-mode . lsp)
         (lsp-mode . ncw/setup-lsp-mode))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-log-io nil)
  (lsp-print-performance nil)
  ;; (lsp-report-if-no-buffer nil)
  ;; (lsp-keep-workspace-alive nil)
  ;; (lsp-enable-snippet t)
  ;; (lsp-auto-guess-root t)
  (lsp-restart 'iteractive)
 ;(lsp-session-file)
  ;; (lsp-auto-configure nil)
 ;(lsp-document-sync-method)
  (lsp-auto-execute-action nil)
  (lsp-eldoce-render-all nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-xref t)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting nil)
  (lsp-before-save-edits nil)
  (lsp-imenu-show-container-name t)
  (lsp-imenu-container-name-separator "/")
  (lsp-imenu-sort-methods '(kind name))
  (lsp-response-timeout 5)
  (lsp-enable-file-watchers nil)
  (lsp-server-trace nil)
  (lsp-semantic-highlighting nil)
  (lsp-enable-imenu t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation nil)
  (lsp-enable-text-document-color nil)
  (lsp-completion-provider :capf)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 3 1024 1024)))


(use-package lsp-ui
  :commands (lsp-ui-mode)
  :custom
  ;; Sideline
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 0)
  ;; Peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory nil)
  ;; Documentation
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.2)
  ;; IMenu
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu--custom-mode-line-format nil)
  :hook (lsp-mode . lsp-ui-mode))


;; (use-package eglot
;;   :ensure nil ;; built-in
;;   :defer t
;;   :after (eldoc)
;;   :hook (((c-mode c++-mode c-ts-mode c++-ts-mode rust-mode rust-ts-mode) . eglot-ensure))
;;   :custom
;;   ;; When no buffers are connected to an LSP server, shut down the server and
;;   ;; eglot, to lighten the load on Emacs.
;;   (eglot-autoshutdown t)
;;   ;; For performance, set this to a low number. When debugging, comment this out.
;;   ;; Setting to 0 means no messages/events are logged in the EGLOT events buffer.
;;   ;; NOTE: In eglot 1.16, this variable was deprecated! If you still want to set
;;   ;; the events buffer size to 0, you need the following:
;;   ;; (setf (plist-get eglot-events-buffer-config :size) 0)
;;   (eglot-events-buffer-size 0)
;;   ;; For performance, set this to ignore. When debugging, comment this out.
;;   ;; fset-ing to ignore means no jsonrpc event are logged by Emacs.
;;   (fset #'jsonrpc--log-event #'ignore)
;;   ;; XRef look-ups can leave the project Eglot is running a server for
;;   (eglot-extend-to-xref t)
;;   ;; Wait some number of seconds before waiting for the connection to the LSP.
;;   ;; With nil, do not wait to connect at all, just try to connect immediately.
;;   (eglot-sync-connect nil)
;;   ;; Reduce the amount of time required for eglot to time-out LSP server
;;   ;; connection attempts.
;;   (eglot-connect-timeout 10)
;;   (eglot-ignored-server-capabilities
;;    '(;; Disable LSP from providing highlighting, since I use treesitter-based or
;;      ;; Emacs' built-in regexp-based major modes for font-locking.
;;      :colorProvider
;;      :documentHighlightProvider
;;      :foldingRangeProvider))
;;   (eglot-stay-out-of '(yasnippet)))

;; (add-hook 'c-ts-mode-hook #'eglot-ensure)
;; (add-hook 'c++-ts-mode-hook #'eglot-ensure)
;; (add-hook 'c-mode-hook #'eglot-ensure)
;; (add-hook 'c++-mode-hook #'eglot-ensure)




(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (racket "https://github.com/6cdh/tree-sitter-racket")))

(setq major-mode-remap-alist nil)
      ;; '((emacs-lisp-mode . emacs-lisp-ts-mode)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(setq ncw/cpp-tsauto-config
      (make-treesit-auto-recipe
         :lang 'cpp
         :ts-mode 'c++-ts-mode
         :remap 'c++-mode
         :requires nil
         :url "https://github.com/tree-sitter/tree-sitter-cpp"
         :revision "v0.22.0"
         :source-dir nil
         :cc nil
         :c++ nil
         :ext "\\.cpp\\'"))

(add-to-list 'treesit-auto-recipe-list ncw/cpp-tsauto-config)




(setq elisp-tsauto-config
      (make-treesit-auto-recipe
       :lang 'elisp
       :ts-mode 'emacs-lisp-ts-mode
       :remap 'emacs-lisp-mode
       :url "https://github.com/Wilfred/tree-sitter-elisp"
       :ext "\\.el\\'"))

(add-to-list 'treesit-auto-recipe-list elisp-tsauto-config)


(use-package evil-textobj-tree-sitter
  :ensure t)

(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

;; Goto start of next function
(define-key evil-normal-state-map
            (kbd "]f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer")))

;; Goto start of previous function
(define-key evil-normal-state-map
            (kbd "[f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))



(define-key evil-normal-state-map (kbd "g r") 'lsp-find-references)
(define-key evil-normal-state-map (kbd "g d") 'lsp-find-declaration)
(define-key evil-normal-state-map (kbd "g d") 'lsp-find-declaration)
(define-key evil-normal-state-map (kbd "g D") 'lsp-find-definition)


(define-key evil-normal-state-map (kbd "K")
      (lambda () (interactive) (lsp-ui-doc-glance)))
;; (lsp-describe-thing-at-point)))



;; (use-package ts-movement
;;   :load-path "lisp/ts-movement"
;;   :config
;;   (define-key ts-movement-map (kbd "C-c .") #'tsm/node-parent)
;;   (define-key ts-movement-map (kbd "C-c ,") #'tsm/node-children)
;;   :hook
;;   (c++-ts-mode . ts-movement-mode)
;;   (c-ts-mode . ts-movement-mode))

;;; Themes




;; Advise load-theme, so that it first disables all custom themes before loading (enabling) another one.
;; https://emacs.stackexchange.com/a/3114
(defadvice load-theme (before theme-dont-propagate activate)
 (mapc #'disable-theme custom-enabled-themes))


(use-package modus-themes :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t))


(defun dark-theme ()
  "Select the dark theme"
  (interactive)
  ;; (load-theme 'doom-horizon t))
  ;; (load-theme 'doom-ayu-dark t))
  (load-theme 'doom-molokai t))
  ;; (load-theme 'modus-vivendi t))
  ;; (load-theme 'vscode-dark-plus t))


(defun light-theme ()
  "Select the light theme"
  (interactive)
  (load-theme 'doom-one-light t))
  ;; (load-theme 'modus-operandi t))

;; Set the dark theme right away
(dark-theme)



;; Pulse the current line when performing certain actions in Emacs.
;; The functions that cause the pulse are in the `pulsar-pulse-functions' list.
(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-delay 0.005))


;;; Keybindings

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys))


(general-create-definer ncw/leader-def
  :states '(normal visual motion emacs insert)
  :keymaps 'override
  :prefix "\\"
  :global-prefix "M-\\")

(ncw/leader-def
  "g" 'magit-status
  ;; "f" 'eglot-format
  "f" 'lsp-format-buffer
  "1" 'dark-theme
  "2" 'light-theme
  "b" 'consult-buffer
  "B" 'switch-to-buffer
  "q" 'delete-window
  "=" 'balance-windows
  "c" 'global-display-line-numbers-mode)

(general-def 'motion
  ";" 'evil-ex
  ":" 'evil-ex
  "q" nil)




(use-package popup
  :ensure t)

(use-package eldoc
  :ensure t)

(defvar-local eldoc-tip--old-eldoc-functions nil
  "The original value of ‘eldoc-display-functions’. The original value before enabling eldoc-box.")

(defface eldoc-tip-tooltip
  '((((class color) (min-colors 88) (background light))
     (:foreground "black" :background "cornsilk"))
    (((class color) (min-colors 88) (background dark))
     (:background "gray26" :foreground "white"))
    (t (:foreground "black" :background "yellow")))

  "Face used for the tooltip.")
(defun eldoc-tip--compose-doc (doc)
  "Compose a doc passed from eldoc.

DOC has the form of (TEXT :KEY VAL...), and KEY can be ‘:thing’
and ‘:face’, among other things. If ‘:thing’ exists, it is put at
the start of the doc followed by a colon. If ‘:face’ exists, it
is applied to the thing.

Return the composed string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    (concat (if thing
                (concat (propertize (format "%s" thing) 'face face) ": ")
              "")
            (car doc))))

;; (defface eldoc-tip-body '((t . nil)))


(defun eldoc-tip--eldoc-message-function (str &rest args)
  "Front-end for eldoc."
  (when (stringp str)
    (let* ((doc (string-trim-right (apply #'format str args))))
       (popup-tip doc
                  :point (point)
                  ;; :min-height 10
                  :max-width 80
                  :truncate nil
                  :margin 2
                  :face 'eldoc-tip-tooltip))))
             ;; :face 'eldoc-tip-body))

(defcustom eldoc-tip-doc-separator "\n----\n"
  "The separator between documentation from different sources.

Since Emacs 28, Eldoc can combine documentation from different
sources, this separator is used to separate documentation from
different sources.

This separator is used for the documentation shown in
‘eldoc-tip-bover-mode’ but not ‘eldoc-box-help-at-point’."
  :type 'string)

(defun eldoc-tip--eldoc-display-function (docs interactive)
  "Display DOCS in childframe.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’. Maybe
display the docs in echo area depending on
‘eldoc-box-only-multi-line’."
  (let ((doc (string-trim (string-join
                              (mapcar #'eldoc-tip--compose-doc docs)
                              eldoc-tip-doc-separator))))
    (when (eldoc-tip--eldoc-message-function "%s" doc)
      (eldoc-display-in-echo-area docs interactive))))


(defun eldoc-tip--enable ()
  "Enable eldoc-tip hover. Intended for internal use."
  (message "eldoc-tip-enable")
  (if (not (boundp 'eldoc-display-functions))
      (add-function :before-while (local 'eldoc-message-function)
                    #'eldoc-tip--eldoc-message-function)

    (setq-local eldoc-tip--old-eldoc-functions eldoc-display-functions)
    (setq-local eldoc-display-functions (list 'eldoc-tip--eldoc-display-function))))
                ;; (cons 'eldoc-tip--eldoc-display-function
                ;;       (remq 'eldoc-display-in-echo-area eldoc-display-functions)))))


(defun eldoc-display-functionsoc-tip--disable ()
  "Disable eldoc-box hover. Intended for internal use."
  (if (not (boundp 'eldoc-display-functions))
      (remove-function (local 'eldoc-message-function) #'eldoc-tip--eldoc-message-function)

    (setq-local eldoc-display-functions
                (remq 'eldoc-tip--eldoc-display-function
                      eldoc-display-functions))
    ;; If we removed eldoc-display-in-echo-area when enabling
    ;; eldoc-box, add it back.
    (when (memq 'eldoc-display-in-echo-area
                eldoc-tip--old-eldoc-functions)
      (setq-local eldoc-display-functions
                  (cons 'eldoc-display-in-echo-area
                        eldoc-display-functions)))))
  ;; (advice-remove #'keyboard-quit #'eldoc-box-quit-frame))

(define-minor-mode eldoc-tip-hover-at-point-mode
  "A convenient minor mode to display doc at point.
You can use \\[keyboard-quit] to hide the doc."
  :lighter " TIP"
  :global t
  (if eldoc-tip-hover-at-point-mode
      (progn (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
             (eldoc-tip--enable))
    (eldoc-tip--disable)))
(require 'eldoc)

;; (eldoc-tip-hover-at-point-mode 1)

;; (general-def 'motion
;;   "K" 'tooltip-eldoc)



;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (global-set-key (kbd "M-o") 'ace-window))

(global-set-key (kbd "M-o") 'other-window)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers nil)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  (company-backends '(company-capf)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer. To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'a.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))










(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package cmake-mode :ensure t)


(defun ncw/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode)
  (auto-fill-mode 0)
  ;; (visual-line-mode 1)
  (set-input-method 'pl-greek)
  (setq evil-auto-indent nil)
  (define-key org-mode-map (kbd "M-<up>") nil)
  (define-key org-mode-map (kbd "M-<left>") nil)
  (define-key org-mode-map (kbd "M-<down>") nil)
  (define-key org-mode-map (kbd "M-<right>") nil))
  ;; (define-key org-mode-map (kbd "M-up")))


(use-package org
  :ensure t
  :hook (org-mode . ncw/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Documents/Tasks.org"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-hide-emphasis-markers t))


(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))


(use-package parinfer-rust-mode
  :hook emacs-lisp-mode racket-mode scheme-mode)



;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "copilot-emacs/copilot.el"
;;                    :branch "main"
;;                    :files ("*.el"))

;;   :custom
;;   (copilot-idle-delay nil)

;;   :hook
;;   ((prog-mode . copilot-mode))

;;   :bind
;;   (("C-c SPC" . copilot-complete)
;;    :map copilot-completion-map
;;    ("TAB" . copilot-accept-completion)
;;    ("<tab>" . copilot-accept-completion)))






;; tmux integration
(use-package tmux-pane
  :ensure t
  :init
  (global-set-key (kbd "M-<up>") #'tmux-pane-omni-window-up)
  (global-set-key (kbd "M-<left>") #'tmux-pane-omni-window-left)
  (global-set-key (kbd "M-<down>") #'tmux-pane-omni-window-down)
  (global-set-key (kbd "M-<right>") #'tmux-pane-omni-window-right)

  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-up)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-left)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-down)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-right))


;; (global-set-key (kbd "M-j") #'next-buffer)
;; (global-set-key (kbd "M-k") #'previous-buffer)


(require 'project)
(global-set-key (kbd "C-p")  #'project-find-file)


(setq-default mode-line-buffer-identification
            '(:eval (propertize "%12b"
                     'face (if (mode-line-window-selected-p)
                             'bold
                            'italic))))



;; (add-to-list 'display-buffer-alist
;;              '((lambda (buffer _) (with-current-buffer buffer
;;                                     (seq-some (lambda (mode)
;;                                                 (derived-mode-p mode))
;;                                               '(help-mode))))
;;                (display-buffer-reuse-window display-buffer-below-selected)
;;                (reusable-frames . visible)
;;                (window-height . 0.25)))




;; (use-package spaceline :ensure t
;;   :config
;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; (use-package spaceline-config :ensure spaceline
;;   :config
;;   (spaceline-helm-mode 1)
;;   (spaceline-spacemacs-theme))


(defun edit-init ()
  "Edit the init.el file in the current buffer"
  (interactive)
  (find-file "~/dotfiles/emacs/init.el"))

(global-set-key (kbd "C-c i")  #'edit-init)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; ====-----------------------------------------------====



;; ====-----------------------------------------------====


(show-paren-mode 1)

;; hitting tab does 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq split-width-threshold 0)
(setq split-height-threshold nil)

;; Enable mouse mode cause I like rats
(xterm-mouse-mode 1)
(mouse-wheel-mode)
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq confirm-kill-emacs #'y-or-n-p)
(setq echo-keystrokes 0.01)


;; Save autosave files to ~/.cache/emacs (this won't work on emacs, but I don't care)
(setq make-backup-files nil) ; stop creating ~ files

(setq create-lockfiles nil) ; don't be annoying

(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)




(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "C-c g d") 'xref-find-definitions)

(defun split-window-right-and-switch ()
  "Split a window right, then switch focus to it."
  (interactive)
  (select-window (split-window-right))
  (balance-windows))


(defun split-window-below-and-switch ()
  "Split a window below, then switch focus to it."
  (interactive)
  (select-window (split-window-below))
  (balance-windows))

(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-\\") #'split-window-right-and-switch)
(global-set-key (kbd "C-_") #'split-window-below-and-switch)
(global-set-key (kbd "C--") #'split-window-below-and-switch)


(add-to-list 'pulsar-pulse-functions #'split-window-right-and-switch)
(add-to-list 'pulsar-pulse-functions #'split-window-below-and-switch)

;; Binding to eval buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; Delete the tailing whitespace whenever you save
(add-hook 'before-save-hook
          (lambda () (interactive)
            "Commands to execute before saving any buffer."
            (delete-trailing-whitespace)))








(defun ncw/title-to-slug (title)
  "Process INPUT by removing symbols, converting to lowercase, and replacing spaces with dashes."
  (let ((cleaned (replace-regexp-in-string "[^[:alnum:] ]" "" title)))
    (downcase (replace-regexp-in-string " " "-" cleaned))))

(defun ncw/new-blog-post (title)
  (interactive "sPost Title: ")
  (let* ((slug (ncw/title-to-slug title))
         (file (format "~/dev/website/posts/%s.md"
                       slug)))
    (with-temp-file file
      (insert (concat "---\n"
                      (format "title: \"%s\"\n" title)
                      (format "date: \"%s\"\n" (format-time-string "%b %d %Y"))
                      "---\n"
                      "\n\n"
                      "# " title "\n")))
    ;; Open the file in the editor
    (find-file-other-window file)))


(defun ncw/blog-paste-image ()
  (interactive)

  (let ((image-slug (format-time-string "%b-%d-%Y-%H-%M-%S.png")))
    ;; Create the image directory
    (shell-command "mkdir -p ~/dev/website/public/post/res/")
    ;; Paste the image to the filesystem
    (shell-command (format "pngpaste ~/dev/website/public/post/res/%s" image-slug))
    ;; And insert the image markdown
    (insert (format "![ALT](/post/res/%s)" image-slug))))


(defun ncw/blog-paste-image-base64 ()
  (interactive)

  (let* ((image-slug (format-time-string "%b-%d-%Y-%H-%M-%S.png"))
         (image-path (format "~/dev/website/raw-images/%s" image-slug)))
    ;; Create the image directory
    (shell-command "mkdir -p ~/dev/website/raw-images")
    ;; Paste the image to the filesystem
    (shell-command (format "pngpaste %s" image-path))
    ;; And insert the image markdown
    (insert (format "![ALT](data:image/png;base64, %s)"
                    (shell-command-to-string (format "base64 --wrap=0 %s"
                                                    image-path))))))
