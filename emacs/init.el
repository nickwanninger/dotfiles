(provide 'init)
;; Make sure we load this file, instead of the precompiled file, if it is newer
(setq load-prefer-newer t)

(straight-use-package 'org)


; (setq use-package-compute-statistics t)

(setq use-package-always-ensure 't
      use-package-always-defer 't)

(setq gc-cons-threshold (* 50 1024 1024))

;; Silence compiler warnings, as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))

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
(use-package no-littering :ensure t)





(when (display-graphic-p)
  (pixel-scroll-precision-mode 1)
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


(if (not (fboundp 'x-hide-tip))
    (defun x-hide-tip ()
      (interactive)))

(require 'cl-lib)




(use-package emacs
  :bind (("RET"      . newline-and-indent)
         ("M-o"      . other-window)
         ("C-c g d"  . xref-find-definitions)
         ("C-c e"    . eval-buffer)

         ;; Control pipe to split to the right
         ("C-\\"     . split-window-right-and-switch)

         ;; Control minus to do a below split
         ("C--"      . split-window-below-and-switch)
         ("C-_"      . split-window-below-and-switch)

         ("M-j"      . previous-window-any-frame)
         ("M-k"      . next-window-any-frame))

  :config

  ;; (global-hl-line-mode 1)
  (show-paren-mode 1)
  ;; Enable mouse mode cause I like rats
  (xterm-mouse-mode 1)
  (mouse-wheel-mode)
  (menu-bar-mode -1)

  (setq resize-mini-windows 'grow-only)
  (setq max-mini-window-height 0.2)
  (setq visible-bell nil)
  (setq ring-bell-function #'ignore)
  ;; Setup the display-buffer-alist
  (setq display-buffer-alist nil)
  ;; don't show ANSI escape sequences in compile buffer
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  ;; (add-to-list 'display-buffer-alist
  (setq compile-command "make -k -j")
  (setq compilation-scroll-output t)

  (setq split-width-threshold 160)
  (setq split-height-threshold nil)
   ;; Setup the display-buffer-alist
  (setq display-buffer-alist nil)
  ;; Enable line numbers
  (global-display-line-numbers-mode)

  ;; Try to use UTF-8 for everything
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8) ;; Catch-all


  ;; hitting tab does 2 spaces
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)



  (setq scroll-conservatively -1)

  (setq enable-local-variables nil)
  (setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))

  (setq use-package-always-ensure t)
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

  ;; Configure the font to be jetbrains in graphical mode
  (when (display-graphic-p))
  ;; (set-frame-font "JetBrains Mono Bold 12" nil t))
  (set-frame-font "Aporetic Sans Mono Bold 14" nil t))


;; Hide the long list of minor modes from the mode-line. The minions
;; package removes all the additional minor-mode names and their
;; information from the mode-line. If I have them all showing, the
;; modeline gets very busy, and very hard to read sometimes. So, I use
;; this package to remove them, leaving only the current major-mode
;; and a ;-) for the rest of the minor modes.
(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

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
  :custom ((dired-listing-switches (if (eq system-type 'darwin)
                                       "-lah"
                                       "-lah --group-directories-first")))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))


(global-set-key (kbd "ESC M-[ a") [M-up])


;; tmux integration
(use-package tmux-pane
  :ensure t
  :bind
  (("M-<up>" . tmux-pane-omni-window-up)
   ("M-<left>" . tmux-pane-omni-window-left)
   ("M-<down>" . tmux-pane-omni-window-down)
   ("M-<right>" . tmux-pane-omni-window-right)

   ("ESC M-[ a" . tmux-pane-omni-window-up)
   ("ESC M-[ d" . tmux-pane-omni-window-left)
   ("ESC M-[ b" . tmux-pane-omni-window-down)
   ("ESC M-[ c" . tmux-pane-omni-window-right)))



(use-package buffer-move
  :ensure t
  :bind
  (("C-c <up>"    . buf-move-up)
   ("C-c <left>"  . buf-move-left)
   ("C-c <down>"  . buf-move-down)
   ("C-c <right>" . buf-move-right)))

(use-package tramp
  :ensure nil
  :config
  (customize-set-variable
    'tramp-ssh-controlmaster-options
     (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
          "-o ControlMaster=auto -o ControlPersist=yes"))
  (setq tramp-default-method "ssh"))



;; A visual undo tree
(use-package vundo :ensure t)




;; (electric-indent-mode -1)


;; clipetty: copy to native clipboard using OSC 52
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))




;; The most important thing we want here is evil mode, cause I can't use emacs without it :)
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)

  :config
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (define-key evil-normal-state-map (kbd ".") #'er/expand-region)
  (define-key evil-visual-state-map (kbd ".") #'er/expand-region)
  (define-key evil-visual-state-map (kbd ",") #'er/contract-region)

  (unless (display-graphic-p)
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q"))))

  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "C-n") 'neotree)
  (define-key evil-normal-state-map (kbd "SPC")
              (lambda ()
                (interactive)
                (evil-execute-macro 1 "viw")))
  (evil-set-leader 'motion (kbd "\\"))
  (setq evil-insert-state-cursor 'bar)

  ;; Alias W to w
  (evil-ex-define-cmd "W" "w")
  ;; :init
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "g r") 'lsp-find-references)
  (define-key evil-normal-state-map (kbd "g d") 'lsp-find-declaration)
  (define-key evil-normal-state-map (kbd "g d") 'lsp-find-declaration)
  (define-key evil-normal-state-map (kbd "g D") 'lsp-find-definition)


  (define-key evil-normal-state-map (kbd "K")
      (lambda () (interactive) (lsp-ui-doc-glance))))

(use-package evil-collection
  :init
  (evil-collection-init))



;; In the terminal, I like to see my mode w/ the cursor shape
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :ensure t
    :init
    (evil-terminal-cursor-changer-activate)))




(cl-defun setup-repl (map &key run-buffer send-to-repl)
  (define-key map (kbd "C-c e") run-buffer)
  (define-key map (kbd "C-c C-r") send-to-repl))

(setup-repl emacs-lisp-mode-map
            :run-buffer #'eval-buffer)


(use-package haskell-mode)
;; (use-package racket-mode
;;   :ensure t
;;   :config
;;    (add-hook 'racket-mode-hook
;;        (lambda ()
;;          (racket-xp-mode)
;;          (set-input-method 'pl-greek)
;;          (setup-repl racket-mode-map
;;                      :run-buffer #'racket-run
;;                      :send-to-repl #'racket-send-last-sexp))))

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


;; magit: the best
(use-package magit
  :ensure t
  :init
  (setq magit-save-repository-buffers 'dontask))


(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

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


(defun ncw/setup-lsp-mode ()
  (message "ncw/setup-lsp-mode called")
  (company-mode 1)
  ;; (lsp-which-key-integration)
  (lsp-diagnostics-mode 1)
  (lsp-completion-mode 1))




;; Yes, I still use lsp-mode.
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-ts-mode . lsp)
         (c-mode . lsp)
         (c++-ts-mode . lsp)
         (python-ts-mode . lsp)
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





(use-package elfeed
  :ensure t
  ;; :bind (("C-c " . #'elfeed))
  :custom
  ;; List of all feeds that I should fetch and care about
  ;; The cdr (tail of list) will be symbols attached to anything coming from that
  ;; particular feed.
  (elfeed-feeds '(("https://karl.hallsby.com/feed.xml")
                  ("https://fasterthanli.me/index.xml")
                  ("https://karthinks.com/index.xml")
                  ("https://wingolog.org/feed/atom")
                  ("https://xkcd.com/atom.xml")
                  ("https://buttondown.email/jaffray/rss")
                  ("https://christine.website/blog.rss")
                  ("https://computer.rip/rss.xml")
                  ("https://nickw.io/api/rss.xml")))

  (elfeed-use-curl t))

;; Prettify the elfeed buffer, making some things easier to read
;; (use-package elfeed-goodies
;;   :ensure t
;;   :after (elfeed)
;;   :config
;;   (elfeed-goodies/setup))









(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (racket "https://github.com/6cdh/tree-sitter-racket")))

(setq major-mode-remap-alist nil)

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; At some point, tree-sitter-cpp broke compat w/ emacs, so I pick a different version
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

;; Add a custom elisp tree-sitter
(setq elisp-tsauto-config
    (make-treesit-auto-recipe
        :lang 'elisp
        :ts-mode 'emacs-lisp-ts-mode
        :remap 'emacs-lisp-mode
        :url "https://github.com/Wilfred/tree-sitter-elisp"
        :ext "\\.el\\'"))

(add-to-list 'treesit-auto-recipe-list elisp-tsauto-config)




;; This is a cpp helper function to easily create definition from declaration
;;   https://gist.github.com/drshapeless/e39ca16d583048e826ec4fa0dfc503b3
(defun ncw/cpp-definition-in-class ()
  "Format the current declaration of function into definition and
put it into kill-ring."
  (interactive)
  (kill-new
   (ncw/generate-cpp-class-function-definition-at-point))
  (message "definition is put in kill-ring"))

(defun ncw/get-cpp-template-node (class-node)
  "Return parent template treesit node.
Return nil if is not in a template."
  (treesit-parent-until class-node
                        (lambda (NODE)
                          (string-equal (treesit-node-type NODE)
                                        "template_declaration"))))

(defun ncw/get-class-function-node-at-point ()
  "Return a treesit node of the current class function."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (NODE)
                          (string-equal (treesit-node-type NODE)
                                        "field_declaration"))
                        t))

(defun ncw/get-cpp-class-node-at-point ()
  "Return the current class treesit node."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (NODE)
                          (let ((NODE-TYPE (treesit-node-type NODE)))
                            (or (string-equal NODE-TYPE
                                              "class_specifier")
                                (string-equal NODE-TYPE
                                              "struct_specifier"))))
                        t))

(defun ncw/generate-cpp-class-function-definition-at-point ()
  "Return the class function definition at point."
  (interactive)
  (string-replace
   ";"
   " {\n\n}"
   (let* ((class-node (ncw/get-cpp-class-node-at-point))
          (func-node  (ncw/get-class-function-node-at-point))
          (template-node (ncw/get-cpp-template-node class-node))
          (class-text (treesit-node-text
                       (treesit-node-child-by-field-name
                        class-node
                        "name")
                       t))
          (func-text (treesit-node-text
                      func-node
                      t))
          (first-space-pos (string-match " "
                                         func-text))
          (insert-pos (string-match "[a-z]"
                                    func-text
                                    first-space-pos)))
     (if template-node
         (let* ((template-parameter (treesit-node-text
                                     (treesit-node-child-by-field-name
                                      template-node
                                      "parameters")
                                     t))
                (template-head (concat "template "
                                       template-parameter
                                       "\n")))


           (concat template-head
                   (substring func-text 0 insert-pos)
                   class-text
                   (string-replace "typename " "" template-parameter)
                   "::"
                   (substring func-text insert-pos)))


       (concat (substring func-text 0 insert-pos)
               class-text
               "::"
               (substring func-text insert-pos))))))





(use-package evil-textobj-tree-sitter
  :ensure t
  :config

  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; Goto start of next function
  (define-key evil-normal-state-map
                (kbd "]f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer"))

    ;; Goto start of previous function
    (define-key evil-normal-state-map
                (kbd "[f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))))



(use-package hl-todo :ensure t
  :config
  ;; (setq hl-todo-color-background t)
  (global-hl-todo-mode))


;;; RefTeX
(use-package reftex
  :ensure nil ;; built-in
  :defer t
  :after (tex-mode)
  ;; Make sure that reftex gets loaded when AucTeX gets loaded, i.e. when LaTeX file is opened
  :hook ((LaTeX-mode . turn-on-reftex)
         (latex-mode . turn-on-reftex))
  :bind (:map latex-mode-map
              ;; Scan the whole document for new labels/citations
              ("C-c r" . reftex-parse-all))
  :custom
  ;; Make RefTeX play nice with AucTeX
  (reftex-plug-into-AUCTeX t)
  ;; When parsing very large documents, we might not want to reparse every file
  (reftex-enable-partial-scans t)
  ;; Set default citation style for RefTeX to use
  (reftex-cite-format 'biblatex)
  ;; Set a default style to present possible citation matches
  (reftex-sort-bibtex-matches 'author))



(use-package auctex
  :ensure t
  :defer t
  :init
  ;; Force auctex (the strictly superior (La)TeX major-mode) to be used by setting
  ;; both the auto-mode-alist and remapping the built-in latex-mode and tex-mode
  ;; major modes to the auctex versions.
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  (add-to-list 'major-mode-remap-alist '(latex-mode . LaTeX-mode))
  (add-to-list 'major-mode-remap-alist '(tex-mode . TeX-mode))
  :custom
  (TeX-parse-self t) ;; Parse multifile documents automagically
  (TeX-auto-save t) ;; Enables parsing upon saving the document
  (TeX-show-compilation t) ;; Always show compilation output
  (TeX-global-PDF-mode t) ;; Make the default TeX mode PDF mode
  (TeX-command-default "pdflatex") ;; Default compile to PDF
  (LaTeX-biblatex-use-Biber t) ;; Make biblatex use Biber automatically
  (TeX-electric-sub-and-superscript t) ;; Inserts {} automaticly on _ and ^
  (TeX-source-correlate-mode t) ;; Correlate output to input so we can easily navigate
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t))


;; Advise load-theme, so that it first disables all custom themes before loading (enabling) another one.
;; https://emacs.stackexchange.com/a/3114
(defadvice load-theme (before theme-dont-propagate activate)
 (mapc #'disable-theme custom-enabled-themes))


(use-package monokai-pro-theme :ensure t)
(use-package ef-themes :ensure t)
(use-package modus-themes :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t))


(defun dark-theme ()
  "Select the dark theme"
  (interactive)
  ;; (load-theme 'doom-monokai-spectrum t))
  (load-theme 'monokai-pro-spectrum t))
  ;; (load-theme 'doom-horizon t))
  ;; (load-theme 'doom-ayu-dark t))
  ;; (load-theme 'doom-molokai t))
  ;; (load-theme 'doom-tokyo-dark t))
  ;; (load-theme 'modus-vivendi t))
  ;; (load-theme 'vscode-dark-plus t))


(defun light-theme ()
  "Select the light theme"
  (interactive)
  ;; (load-theme 'doom-nord-light t))
  (load-theme 'modus-operandi t))
  ;; (load-theme 'modus-operandi-tinted t))

;; Set the dark theme right away
(dark-theme)






(defun ncw/project-make (&optional cmd)
  "Run `make -k -j' in the project root."
  ; (declare (interactive-only compile))
  (interactive)
  (unless cmd (setq cmd "make -k -j"))
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (compile cmd)
    (let ((compile-window (get-buffer-window "*compilation*")))
      (when compile-window
        (select-window compile-window)))))


;;; Keybindings

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  :init
  (general-create-definer ncw/leader-def
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "\\"
    :global-prefix "M-\\")

  (ncw/leader-def
    "g" (lambda () (interactive)
          (magit-status)
          (balance-windows))
    ;; "f" 'eglot-format
    "e" 'elfeed
    "f" 'lsp-format-buffer
    "r" 'transpose-frame ;; from transpose-frame below
    "1" 'dark-theme
    "2" 'light-theme
    "b" 'consult-buffer
    "B" 'switch-to-buffer
    "q" 'delete-window
    "\\" 'balance-windows
    "=" 'balance-windows
    "c" 'global-display-line-numbers-mode
    "z" 'vundo ;; open visual undo mode
    "m" 'ncw/project-make
    "I" 'ncw/cpp-definition-in-class
    "t" 'vterm)

  (general-def 'motion
    ";" 'evil-ex
    ":" 'evil-ex
    "q" nil))




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
  (visual-line-mode 1)
  ;; (set-input-method 'pl-greek)
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


(use-package transpose-frame)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t

  ;; :custom
  ;; (copilot-idle-delay nil)

  :hook
  ((prog-mode . copilot-mode))

  :bind
  (("C-c SPC" . copilot-complete)
   :map copilot-completion-map
   ("TAB" . copilot-accept-completion)
   ("<tab>" . copilot-accept-completion)))

;; Bind tab so that if there is a company suggestion, suggest that. otherwise do copilot-complete
 



;; Vterm: a decent terminal emulator for emacs
(use-package vterm
    :ensure t
    :hook (vterm-mode-hook . (lambda () (display-line-numbers-mode 0)))
    :config ;; config us run *after* the package is loaded
    ;; (unbind-key "M-<up>" vterm-mode-map)
    ;; (unbind-key "M-<down>" vterm-mode-map)
    ;; (unbind-key "M-<left>" vterm-mode-map)
    ;; (unbind-key "M-<right>" vterm-mode-map)
    (defun evil-collection-vterm-escape-stay ()
        "Go back to normal state but don't move cursor backwards. Moving cursor
         backwards is the default vim behavior but it is not appropriate in some
         cases like terminals."
        (setq-local evil-move-cursor-back nil))

    (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay))






(use-package project
  :config
  (setq project-vc-merge-submodules nil))


(setq-default mode-line-buffer-identification
            '(:eval (propertize "%12b"
                     'face (if (mode-line-window-selected-p)
                             'bold
                            'italic))))

(defun edit-init ()
  "Edit the init.el file in the current buffer"
  (interactive)
  (find-file "~/dotfiles/emacs/init.el"))

(global-set-key (kbd "C-c i")  #'edit-init)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;; Pulse the current line when performing certain actions in Emacs.
;; The functions that cause the pulse are in the `pulsar-pulse-functions' list.
(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode)
  (add-to-list 'pulsar-pulse-functions #'split-window-right-and-switch)
  (add-to-list 'pulsar-pulse-functions #'split-window-below-and-switch)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-up)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-left)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-down)
  (add-to-list 'pulsar-pulse-functions #'tmux-pane-omni-window-right)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-delay 0.005))


;; Binding to eval buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; Delete the tailing whitespace whenever you save
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)








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


(defun ncw/build-staged-done (buffer msg)
  (when (string-match "finished" msg)
    (magit-stash-pop)))

(defun ncw/build-staged ()
  (interactive)
  (magit-stash-push)
  (ncw/project-make "make -k -j; git stash pop"))
