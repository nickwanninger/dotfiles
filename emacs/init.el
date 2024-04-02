(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)


(defun ensure-package (name)
  "Ensure a package is installed"
  (unless (package-installed-p name)
    (package-install name)))



(use-package magit)


;; Modus Theme
(ensure-package 'modus-themes)
(load-theme 'modus-operandi 't)


(setq evil-want-keybinding nil)
;; Evil Mode
(use-package evil
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


(ensure-package 'ace-window)
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


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



;; tmux integration
(use-package tmux-pane
  :ensure t
  :bind (("M-<up>" . tmux-pane-omni-window-up)
         ("M-<left>" . tmux-pane-omni-window-left)
         ("M-<down>" . tmux-pane-omni-window-down)
         ("M-<right>" . tmux-pane-omni-window-right)))




(use-package racket-mode
  :ensure t
  :config
   (add-hook 'racket-mode-hook
	     (lambda ()
		(racket-xp-mode)
		(define-key racket-mode-map (kbd "C-c r") 'racket-run))))

(use-package org
  :ensure t)




;; ====-----------------------------------------------====


;; Enable mouse mode cause I like rats
(xterm-mouse-mode 1)
(mouse-wheel-mode)
(menu-bar-mode -1)

(when (display-graphic-p)
  (scroll-bar-mode -1))

(setq confirm-kill-emacs #'y-or-n-p)
(setq echo-keystrokes 0.01)
(global-display-line-numbers-mode -1)


(setq make-backup-files nil) ; stop creating ~ files




(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)



(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "C-_") 'split-window-below)
