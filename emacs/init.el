(provide 'init)
;; Make sure we load this file, instead of the precompiled file, if it is newer
(setq load-prefer-newer t)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Setup the package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
   (package-refresh-contents))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
             :ensure t)



;; Try to use UTF-8 for everything
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ;; Catch-all


(require 'pl-greek)


(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)






(defun ensure-package (name)
  "Ensure a package is installed"
  (unless (package-installed-p name)
    (package-install name)))

;; 
(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package magit)

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
  :ensure t)


(global-set-key (kbd "C-c f") #'consult-ripgrep)


;; ====--------------------------------------------------====


(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-vivendi 't))


(setq evil-want-keybinding nil)
;; (setq evil-want-C-i-jump nil)

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
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

(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(use-package eldoc-box
   :ensure t)


(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window))


;; Switch between modus themes
(global-set-key (kbd "C-x C-h 1") 'modus-themes-toggle)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers t)
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







(use-package racket-mode
  :ensure t
  :config
   (add-hook 'racket-mode-hook
       (lambda ()
         (racket-xp-mode)
         (set-input-method 'pl-greek)
         (define-key racket-mode-map (kbd "C-c r") 'racket-run))))




(defun ncw/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
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


(use-package parinfer-rust-mode
  :hook emacs-lisp-mode racket-mode)


;; tmux integration
(use-package tmux-pane
  :ensure t
  :init
  (global-set-key (kbd "M-<up>") #'tmux-pane-omni-window-up)
  (global-set-key (kbd "M-<left>") #'tmux-pane-omni-window-left)
  (global-set-key (kbd "M-<down>") #'tmux-pane-omni-window-down)
  (global-set-key (kbd "M-<right>") #'tmux-pane-omni-window-right))

  
  

(require 'project)
(global-set-key (kbd "C-p")  #'project-find-file)



(defun edit-init ()
  (interactive)
  (find-file "~/dotfiles/emacs/init.el"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 


;; ====-----------------------------------------------====



;; ====-----------------------------------------------====


(show-paren-mode 1)

;; hitting tab does 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-always-indent 'complete)

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
(global-display-line-numbers-mode -1)


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
  (select-window (split-window-right)))


(defun split-window-below-and-switch ()
  "Split a window below, then switch focus to it."
  (interactive)
  (select-window (split-window-below)))

(global-set-key (kbd "C-\\") #'split-window-right-and-switch)
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-_") #'split-window-below-and-switch)
(global-set-key (kbd "C--") #'split-window-below-and-switch)


;; Binding to eval buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; Delete the tailing whitespace whenever you save
(add-hook 'before-save-hook
          (lambda () (interactive)
            "Commands to execute before saving any buffer."
            (delete-trailing-whitespace)))
