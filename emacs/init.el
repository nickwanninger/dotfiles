(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))



(defun ensure-package (name)
  "Ensure a package is installed"
  (unless (package-installed-p name)
    (package-install name)))


(ensure-package 'magit)


;; Modus Theme
(ensure-package 'modus-themes)
(load-theme 'modus-operandi 't)





;; Evil Mode
(ensure-package 'evil)
(require 'evil)

(evil-mode 1)

(add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
(add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))

(setq evil-insert-state-cursor 'bar)

(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)


(ensure-package 'ace-window)
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


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
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Enable mouse mode cause I like rats
(xterm-mouse-mode 1)
(mouse-wheel-mode)

(setq confirm-kill-emacs #'y-or-n-p)
(setq echo-keystrokes 0.01)
(global-display-line-numbers-mode)


(setq make-backup-files nil) ; stop creating ~ files


(menu-bar-mode -1) 


(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)



(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "C-_") 'split-window-below)
