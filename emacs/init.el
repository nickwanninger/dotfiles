(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages '(magit)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
;; (load-theme 'modus-vivendi)
(setq confirm-kill-emacs 'y-or-n-p)
