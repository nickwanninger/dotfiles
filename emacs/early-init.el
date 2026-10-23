;;; early-init.el --- Early settings -*- lexical-binding: t; -*-

;; Compile Lisp in the background with one worker.
(setq native-comp-speed 2
      native-comp-jit-compilation t
      native-comp-async-jobs-number 1)

;; Disable package.el because Straight manages packages.
(setq package-enable-at-startup nil)
(setq straight-base-dir (concat "~/.emacs.d/straight/" emacs-version))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)


(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET"
          (car (process-lines "/usr/bin/sw_vers" "-productVersion"))))
