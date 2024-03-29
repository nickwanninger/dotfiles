(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))



(defun ensure-package (name)
  "Ensure a package is installed"
  (unless (package-installed-p name)
    (package-install name)))


(ensure-package 'evil)
(ensure-package 'magit)
(ensure-package 'modus-themes)


(require 'evil)
(evil-mode 1)


(load-theme 'modus-operandi 't)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
;; (load-theme 'modus-vivendi)
(setq confirm-kill-emacs nil)
(setq echo-keystrokes 0.01)

;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
