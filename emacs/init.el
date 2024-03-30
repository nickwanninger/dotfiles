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




;; LSP mode

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (prog-mode . lsp-deferred)
;;   :custom
;;   (lsp-clients-clangd-executable "ccls") ;; or use ccls package to get call
;;                                          ;; hierarchy lsp extension
;;   (lsp-auto-guess-root t)                ;; auto guess root
;;   (lsp-prefer-capf t)                    ;; using `company-capf' by default
;;   (lsp-keymap-prefix "C-c l"))

;; (use-package company
;;   :ensure t
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-mode-map
;;          ([remap completion-at-point] . company-complete))
;;   :custom
;;   (company-idle-delay 0)
;;   (company-echo-delay 0)
;;   (company-show-numbers t)
;;   (company-require-match nil)
;;   (company-tooltip-align-annotations t)
;;   (company-backends '(company-capf)))


(add-hook 'c++-mode-hook 'lsp)

;; Enable mouse mode cause I like rats
(xterm-mouse-mode 1)

(setq confirm-kill-emacs #'y-or-n-p)
(setq echo-keystrokes 0.01)
(global-display-line-numbers-mode)


(setq make-backup-files nil) ; stop creating ~ files


(menu-bar-mode -1) 
